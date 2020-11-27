{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Agent.Socket
  ( withReliableSocket,
    checkVersion',
    Socket (..),
    syncIO,
    SocketConfig (..),
  )
where

import Control.Concurrent.STM.TBQueue (TBQueue, flushTBQueue, newTBQueue, writeTBQueue)
import Control.Concurrent.STM.TChan (TChan, writeTChan)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVar, readTVar, writeTVar)
import Control.Monad.IO.Unlift
import qualified Data.Aeson as A
import Data.DList (DList, fromList)
import Data.List (dropWhileEnd, splitAt)
import Data.Semigroup
import Data.Time (NominalDiffTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Extras
import Hercules.API.Agent.LifeCycle.ServiceInfo (ServiceInfo)
import qualified Hercules.API.Agent.LifeCycle.ServiceInfo as ServiceInfo
import qualified Hercules.API.Agent.Socket.Frame as Frame
import Hercules.API.Agent.Socket.Frame (Frame)
import Hercules.Agent.STM (atomically, newTChanIO, newTVarIO)
import Katip (KatipContext, Severity (..), katipAddContext, katipAddNamespace, logLocM, sl)
import Network.URI (URI, uriAuthority, uriPath, uriPort, uriQuery, uriRegName, uriScheme)
import Network.WebSockets (Connection, runClientWith)
import qualified Network.WebSockets as WS
import Protolude hiding (atomically, handle, race, race_)
import UnliftIO.Async (race, race_)
import UnliftIO.Exception (handle)
import UnliftIO.Timeout (timeout)
import Wuss (runSecureClientWith)

data Socket r w
  = Socket
      { write :: w -> STM (),
        serviceChan :: TChan r,
        sync :: STM (STM ())
      }

syncIO :: Socket r w -> IO ()
syncIO = join . fmap atomically . atomically . sync

-- | Parameters to start 'withReliableSocket'.
data SocketConfig ap sp m
  = SocketConfig
      { makeHello :: m ap,
        checkVersion :: (sp -> m (Either Text ())),
        baseURL :: URI,
        path :: Text,
        token :: ByteString
      }

requiredServiceVersion :: (Int, Int)
requiredServiceVersion = (2, 0)

ackTimeout :: NominalDiffTime
ackTimeout = 60 -- seconds

withReliableSocket :: (A.FromJSON sp, A.ToJSON ap, MonadIO m, MonadUnliftIO m, KatipContext m) => SocketConfig ap sp m -> (Socket sp ap -> m a) -> m a
withReliableSocket socketConfig f = do
  writeQueue <- atomically $ newTBQueue 100
  agentMessageNextN <- newTVarIO 0
  serviceMessageChan <- newTChanIO
  highestAcked <- newTVarIO (-1)
  let tagPayload p = do
        c <- readTVar agentMessageNextN
        writeTVar agentMessageNextN (c + 1)
        pure $ Frame.Msg {n = c, p = p}
      socketThread = runReliableSocket socketConfig writeQueue serviceMessageChan highestAcked
      socket = Socket
        { write = tagPayload >=> writeTBQueue writeQueue,
          serviceChan = serviceMessageChan,
          sync = do
            counterAtSyncStart <- (\n -> n - 1) <$> readTVar agentMessageNextN
            pure do
              acked <- readTVar highestAcked
              guard $ acked >= counterAtSyncStart
        }
  race socketThread (f socket) <&> either identity identity

checkVersion' :: Applicative m => ServiceInfo -> m (Either Text ())
checkVersion' si =
  if ServiceInfo.version si < requiredServiceVersion
    then pure $ Left $ "Expected service version " <> show requiredServiceVersion
    else pure $ Right ()

runReliableSocket :: forall ap sp m. (A.ToJSON ap, A.FromJSON sp, MonadUnliftIO m, KatipContext m) => SocketConfig ap sp m -> TBQueue (Frame ap ap) -> TChan sp -> TVar Integer -> forall a. m a
runReliableSocket socketConfig writeQueue serviceMessageChan highestAcked = katipAddNamespace "Socket" do
  expectedAck <- liftIO $ newTVarIO Nothing
  (unacked :: TVar (DList (Frame Void ap))) <- atomically $ newTVar mempty
  (lastServiceN :: TVar Integer) <- atomically $ newTVar (-1)
  let katipExceptionContext e =
        katipAddContext (sl "message" (displayException e))
          . katipAddContext (sl "exception" (show e :: [Char]))
      logWarningPause :: SomeException -> m ()
      logWarningPause e | Just (WS.ConnectionClosed) <- fromException e = do
        katipExceptionContext e $ logLocM InfoS "Socket closed. Reconnecting."
        liftIO $ threadDelay 10_000_000
      logWarningPause e | Just (WS.ParseException "not enough bytes") <- fromException e = do
        katipExceptionContext e $ logLocM InfoS "Socket closed prematurely. Reconnecting."
        liftIO $ threadDelay 10_000_000
      logWarningPause e = do
        katipExceptionContext e $ logLocM WarningS "Recovering from exception in socket handler. Reconnecting."
        liftIO $ threadDelay 10_000_000
      setExpectedAckForMsgs :: [Frame ap ap] -> m ()
      setExpectedAckForMsgs msgs =
        msgs
          & foldMap (\case Frame.Msg {n = n} -> Option $ Just $ Max n; _ -> mempty)
          & traverse_ (\(Max n) -> setExpectedAck n)
      send :: Connection -> [Frame ap ap] -> m ()
      send conn = sendSorted . sortBy (compare `on` msgN)
        where
          sendRaw :: [Frame ap ap] -> m ()
          sendRaw msgs = do
            liftIO $ WS.sendDataMessages conn (WS.Binary . A.encode <$> msgs)
            setExpectedAckForMsgs msgs
          sendSorted :: [Frame ap ap] -> m ()
          sendSorted [] = pass
          sendSorted msgs = do
            let (msgsNow, msgsLater) = Data.List.splitAt 100 msgs
            sendRaw msgsNow
            sendSorted msgsLater
      recv :: Connection -> m (Frame sp sp)
      recv conn = do
        withTimeout ackTimeout (FatalError "Hercules.Agent.Socket.recv timed out") $
          (liftIO $ A.eitherDecode <$> WS.receiveData conn) >>= \case
            Left e -> liftIO $ throwIO (FatalError $ "Error decoding service message: " <> toS e)
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
        unackedNow <- atomically $ readTVar unacked
        send conn $ fmap (Frame.mapOob absurd) $ toList unackedNow
      cleanAcknowledged newAck = atomically do
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
        modifyTVar highestAcked (max newAck)
      -- TODO (performance) IntMap?

      readThread conn = katipAddNamespace "Reader" do
        forever $ do
          msg <- recv conn
          case msg of
            Frame.Msg {p = pl, n = n} -> atomically do
              lastN <- readTVar lastServiceN
              -- when recent
              when (n > lastN) do
                writeTChan serviceMessageChan pl
                writeTVar lastServiceN n
              writeTBQueue writeQueue (Frame.Ack {n = n})
            Frame.Ack {n = n} ->
              cleanAcknowledged n
            Frame.Oob o -> atomically do
              writeTChan serviceMessageChan o
            Frame.Exception e -> katipAddContext (sl "message" e) $ logLocM WarningS $ "Service exception"
      writeThread conn = katipAddNamespace "Writer" do
        forever do
          msgs <- atomically do
            -- TODO: make unacked bounded
            allMessages <- flushTBQueue writeQueue
            when (null allMessages) retry
            modifyTVar unacked (<> (allMessages >>= Frame.removeOob & fromList))
            pure allMessages
          send conn msgs
      setExpectedAck :: Integer -> m ()
      setExpectedAck n = do
        now <- liftIO getCurrentTime
        atomically do
          writeTVar expectedAck $ Just $ (n, now)
      noAckCleanupThread = noAckCleanupThread' (-1)
      noAckCleanupThread' confirmedLastTime = do
        (expectedN, sendTime) <- atomically do
          readTVar expectedAck
            >>= \case
              Nothing -> retry
              Just (expectedN, _) | expectedN <= confirmedLastTime -> retry
              Just a -> pure a
        let expectedArrival = ackTimeout `addUTCTime` sendTime
        liftIO do
          now <- getCurrentTime
          let waitTime = expectedArrival `diffUTCTime` now
          delayNominalDiffTime waitTime
        currentHighestAck <- atomically do
          readTVar highestAcked
        if expectedN > currentHighestAck
          then do
            katipAddContext (sl "expectedAck" expectedN <> sl "highestAck" currentHighestAck) do
              logLocM Katip.DebugS "Did not receive ack in time. Will reconnect."
            -- terminate other threads via race_
            pass
          else noAckCleanupThread' expectedN
  forever
    $ handle logWarningPause
    $ withConnection' socketConfig
    $ \conn -> do
      katipAddNamespace "Handshake" do
        handshake conn
      readThread conn `race_` writeThread conn `race_` noAckCleanupThread

msgN :: Frame o a -> Maybe Integer
msgN (Frame.Msg {n = n}) = Just n
msgN _ = Nothing

withConnection' :: (MonadUnliftIO m) => SocketConfig any0 any1 m -> (Connection -> m a) -> m a
withConnection' socketConfig m = do
  UnliftIO unlift <- askUnliftIO
  let opts = WS.defaultConnectionOptions
      headers = [("Authorization", "Bearer " <> token socketConfig)]
      base = baseURL socketConfig
      url = base {uriPath = uriPath base `slash` toS (path socketConfig)}
      defaultPort
        | uriScheme url == "http:" = 80
        | uriScheme url == "https:" = 443
        | otherwise = panic "Hercules.Agent.Socket: invalid uri scheme"
      port = fromMaybe defaultPort $ do
        auth <- uriAuthority url
        readMaybe $ dropWhile (== ':') $ uriPort auth
      regname = fromMaybe (panic "Hercules.Agent.Socket: url has no regname") $ do
        auth <- uriAuthority url
        pure $ uriRegName auth
      httpPath = uriPath url <> uriQuery url
      runSocket
        | uriScheme url == "http:" = runClientWith regname port httpPath opts headers
        | uriScheme url == "https:" = runSecureClientWith regname (fromIntegral port) httpPath opts headers
        | otherwise = panic "Hercules.Agent.Socket: invalid uri scheme"
  liftIO $ runSocket $ \conn -> unlift (m conn)

slash :: [Char] -> [Char] -> [Char]
a `slash` b = dropWhileEnd (== '/') a <> "/" <> dropWhile (== '/') b

withTimeout :: (Exception e, MonadIO m, MonadUnliftIO m) => NominalDiffTime -> e -> m a -> m a
withTimeout t e _ | t <= 0 = throwIO e
withTimeout t e m = timeout (ceiling $ t * 1_000_000) m >>= \case
  Nothing -> throwIO e
  Just a -> pure a
