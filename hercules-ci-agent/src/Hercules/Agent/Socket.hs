{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Agent.Socket
  ( withReliableSocket,
    checkVersion',
    Socket (..),
    SocketConfig (..),
  )
where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, flushTBQueue, newTBQueue, writeTBQueue)
import Control.Concurrent.STM.TChan (TChan, newTChan, writeTChan)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVar, readTVar, writeTVar)
import Control.Monad.IO.Unlift
import qualified Data.Aeson as A
import Data.DList (DList, fromList)
import Hercules.API.Agent.LifeCycle.ServiceInfo (ServiceInfo)
import qualified Hercules.API.Agent.LifeCycle.ServiceInfo as ServiceInfo
import qualified Hercules.API.Agent.Socket.Frame as Frame
import Hercules.API.Agent.Socket.Frame (Frame)
import Katip (KatipContext, Severity (..), katipAddContext, katipAddNamespace, logLocM, sl)
import Network.WebSockets (Connection, runClientWith)
import qualified Network.WebSockets as WS
import Protolude hiding (handle, race, race_)
import UnliftIO.Async (race, race_)
import UnliftIO.Exception (handle)
import Wuss (runSecureClientWith)

data Socket r w
  = Socket
      { write :: w -> STM (),
        serviceChan :: TChan r
      }

-- | Parameters to start 'withReliableSocket'.
data SocketConfig ap sp m
  = SocketConfig
      { makeHello :: m ap,
        checkVersion :: (sp -> m (Either Text ())),
        host :: Text,
        path :: Text,
        token :: ByteString
      }

requiredServiceVersion :: (Int, Int)
requiredServiceVersion = (1, 0)

-- | @'liftIO' . 'atomically'@
a'ly :: MonadIO m => STM a -> m a
a'ly = liftIO . atomically

withReliableSocket :: (A.FromJSON sp, A.ToJSON ap, MonadIO m, MonadUnliftIO m, KatipContext m) => SocketConfig ap sp m -> (Socket sp ap -> m a) -> m a
withReliableSocket socketConfig f = do
  writeQueue <- a'ly $ newTBQueue 100
  agentMessageCounter <- a'ly $ newTVar 0
  serviceMessageChan <- a'ly $ newTChan
  let tagPayload p = do
        c <- readTVar agentMessageCounter
        writeTVar agentMessageCounter (c + 1)
        pure $ Frame.Msg {n = c, p = p}
      socketThread = runReliableSocket socketConfig writeQueue serviceMessageChan
      socket = Socket
        { write = tagPayload >=> writeTBQueue writeQueue,
          serviceChan = serviceMessageChan
        }
  race socketThread (f socket) <&> either identity identity

checkVersion' :: Applicative m => ServiceInfo -> m (Either Text ())
checkVersion' si =
  if ServiceInfo.version si < requiredServiceVersion
    then pure $ Left $ "Expected service version " <> show requiredServiceVersion
    else pure $ Right ()

runReliableSocket :: forall ap sp m. (A.ToJSON ap, A.FromJSON sp, MonadUnliftIO m, KatipContext m) => SocketConfig ap sp m -> TBQueue (Frame ap ap) -> TChan sp -> forall a. m a
runReliableSocket socketConfig writeQueue serviceMessageChan = katipAddNamespace "Socket" do
  (unacked :: TVar (DList (Frame Void ap))) <- a'ly $ newTVar mempty
  (lastServiceN :: TVar Integer) <- a'ly $ newTVar (-1)
  let logWarningPause :: SomeException -> m ()
      logWarningPause e = do
        katipAddContext (sl "message" (displayException e))
          $ katipAddContext (sl "exception" (show e :: [Char]))
          $ logLocM WarningS "Recovering from exception in socket handler"
        liftIO $ threadDelay 10_000_000
      send :: Connection -> [Frame ap ap] -> m ()
      send conn msgs = do
        liftIO $ WS.sendDataMessages conn (WS.Binary . A.encode <$> msgs)
      recv :: Connection -> m (Frame sp sp)
      recv conn = do
        (liftIO $ A.eitherDecode <$> WS.receiveData conn) >>= \case
          Left e -> liftIO $ throwIO (FatalError $ "Error decoding service message: " <> toSL e)
          Right r -> pure r
      handshake conn = katipAddNamespace "Handshake" do
        siMsg <- recv conn
        case siMsg of
          Frame.Oob {o = o'} -> checkVersion socketConfig o' >>= \case
            Left e -> do
              send conn [Frame.Exception e]
              throwIO $ FatalError "It looks like you're running a development version of hercules-ci-agent that is not yet supported on hercules-ci.com. Please use the stable branch or a tag."
            Right _ -> pass
          _ -> throwIO $ FatalError "Unexpected message. This is either a bug or you might need to update your agent."
        hello <- makeHello socketConfig
        send conn [Frame.Oob hello]
        ackMsg <- recv conn
        case ackMsg of
          Frame.Ack {n = n} -> cleanAcknowledged n
          _ -> throwIO $ FatalError "Expected acknowledgement. This is either a bug or you might need to update your agent."
        sendUnacked conn
      sendUnacked :: Connection -> m ()
      sendUnacked conn = do
        unackedNow <- a'ly $ readTVar unacked
        send conn $ fmap (Frame.mapOob absurd) $ toList unackedNow
      cleanAcknowledged newAck = a'ly do
        unacked0 <- readTVar unacked
        writeTVar unacked $
          unacked0
            & toList
            & filter
              ( \umsg -> case umsg of
                  Frame.Msg {n = n} -> n > newAck
                  Frame.Oob x -> absurd x
                  Frame.Ack {} -> False
                  Frame.Exception {} -> False
              )
            & fromList
      -- TODO (performance) IntMap?

      readThread conn = katipAddNamespace "Reader" do
        forever $ do
          msg <- recv conn
          case msg of
            Frame.Msg {p = pl, n = n} -> a'ly do
              lastN <- readTVar lastServiceN
              -- when recent
              when (n > lastN) do
                writeTChan serviceMessageChan pl
                writeTVar lastServiceN n
              writeTBQueue writeQueue (Frame.Ack {n = n})
            Frame.Ack {n = n} ->
              cleanAcknowledged n
            Frame.Oob o -> a'ly do
              writeTChan serviceMessageChan o
            Frame.Exception e -> katipAddContext (sl "message" e) $ logLocM WarningS $ "Service exception"
      writeThread conn = katipAddNamespace "Writer" do
        forever do
          msgs <- a'ly do
            -- TODO: make unacked bounded
            allMessages <- flushTBQueue writeQueue
            when (null allMessages) retry
            modifyTVar unacked (<> (allMessages >>= Frame.removeOob & fromList))
            pure allMessages
          send conn msgs
  forever
    $ handle logWarningPause
    $ withConnection' socketConfig
    $ \conn -> do
      katipAddNamespace "Handshake" do
        handshake conn
      race_ (readThread conn) (writeThread conn)

withConnection' :: (MonadUnliftIO m) => SocketConfig any0 any1 m -> (Connection -> m a) -> m a
withConnection' socketConfig m = do
  UnliftIO unlift <- askUnliftIO
  let opts = WS.defaultConnectionOptions
      headers = [("Authorization", "Bearer " <> token socketConfig)]
      host' = host socketConfig
      path' = toS $ path socketConfig
      runSocket
        | host' == "test://api" = runClientWith "api" 80 path' opts headers
        | otherwise = runSecureClientWith (toS host') 443 path' opts headers
  liftIO $ runSocket $ \conn -> unlift (m conn)
