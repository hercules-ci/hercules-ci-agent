{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Agent.Socket
  ( withReliableSocket,
    Socket (..),
  )
where

import Control.Concurrent.Async.Lifted (race, race_)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, flushTBQueue, newTBQueue, writeTBQueue)
import Control.Concurrent.STM.TChan (TChan, newTChan, writeTChan)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVar, readTVar, writeTVar)
import Control.Monad.IO.Unlift
import qualified Data.Aeson as A
import Data.DList (DList, fromList)
import Data.IORef.Lifted
import qualified Data.Map as M
import Hercules.API.Agent.LifeCycle.ServiceInfo (ServiceInfo)
import qualified Hercules.API.Agent.LifeCycle.ServiceInfo as ServiceInfo
import Hercules.API.Agent.LifeCycle.StartInfo (Hello, tasksInProgress)
import Hercules.API.Agent.Socket.AgentPayload (AgentPayload)
import qualified Hercules.API.Agent.Socket.AgentPayload as AgentPayload
import qualified Hercules.API.Agent.Socket.Frame as Frame
import Hercules.API.Agent.Socket.Frame (Frame)
import Hercules.API.Agent.Socket.ServicePayload (ServicePayload)
import qualified Hercules.API.Agent.Socket.ServicePayload as ServicePayload
import Hercules.API.Id
import Hercules.API.Task as Task
import Hercules.Agent.Config
import Hercules.Agent.Env (App, config, currentToken)
import qualified Hercules.Agent.Env as Agent.Env
import Hercules.Agent.Log
import Hercules.Agent.Socket.Env
import Network.WebSockets (Connection, runClientWith)
import qualified Network.WebSockets as WS
import Protolude hiding (handle, race, race_)
import qualified Servant.Auth.Client
import UnliftIO.Exception (handle)
import Wuss (runSecureClientWith)

requiredServiceVersion :: (Int, Int)
requiredServiceVersion = (1, 0)

-- | @'liftIO' . 'atomically'@
a'ly :: STM a -> App a
a'ly = liftIO . atomically

withReliableSocket :: Hello -> IORef (Map (Id (Task Task.Any)) b) -> (Socket -> App a) -> App a
withReliableSocket hello tasks f = do
  writeQueue <- a'ly $ newTBQueue 100
  agentMessageCounter <- a'ly $ newTVar 0
  serviceMessageChan <- a'ly $ newTChan
  let tagPayload p = do
        c <- readTVar agentMessageCounter
        writeTVar agentMessageCounter (c + 1)
        pure $ Frame.Msg {n = c, p = p}
      socketThread = runReliableSocket hello tasks writeQueue serviceMessageChan
      socket = Socket
        { write = tagPayload >=> writeTBQueue writeQueue,
          serviceChan = serviceMessageChan
        }
      setSocket env = env {Agent.Env.socket = socket}
  race socketThread (local setSocket $ f socket) <&> either identity identity

runReliableSocket :: Hello -> IORef (Map (Id (Task Task.Any)) b) -> TBQueue (Frame AgentPayload AgentPayload) -> TChan (ServicePayload) -> App a
runReliableSocket hello tasks writeQueue serviceMessageChan = katipAddNamespace "Socket" do
  base <- asks (agentSocketBase . config)
  token <- asks (Servant.Auth.Client.getToken . currentToken)
  (unacked :: TVar (DList (Frame Void AgentPayload))) <- a'ly $ newTVar mempty
  (lastServiceN :: TVar Integer) <- a'ly $ newTVar (-1)
  let logWarningPause :: SomeException -> App ()
      logWarningPause e = do
        withNamedContext "message" (displayException e)
          $ withNamedContext "exception" (show e :: [Char])
          $ logLocM WarningS "Recovering from exception in socket handler"
        liftIO $ threadDelay 10_000_000
      send :: Connection -> [Frame AgentPayload AgentPayload] -> App ()
      send conn msgs = do
        liftIO $ WS.sendDataMessages conn (WS.Binary . A.encode <$> msgs)
      recv :: Connection -> App (Frame ServicePayload ServicePayload)
      recv conn = do
        (liftIO $ A.eitherDecode <$> WS.receiveData conn) >>= \case
          Left e -> liftIO $ throwIO (FatalError $ "Error decoding service message: " <> toSL e)
          Right r -> pure r
      handshake conn = katipAddNamespace "Handshake" do
        siMsg <- recv conn
        case siMsg of
          Frame.Oob {o = ServicePayload.ServiceInfo si} -> checkVersion conn si
          _ -> throwIO $ FatalError "Unexpected message. This is either a bug or you might need to update your agent."
        currentTasks <- readIORef tasks
        send conn [Frame.Oob $ AgentPayload.Hello hello {tasksInProgress = M.keys currentTasks}]
        ackMsg <- recv conn
        case ackMsg of
          Frame.Ack {n = n} -> cleanAcknowledged n
          _ -> throwIO $ FatalError "Expected acknowledgement. This is either a bug or you might need to update your agent."
        sendUnacked conn
      sendUnacked :: Connection -> App ()
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

      checkVersion :: Connection -> ServiceInfo -> App ()
      checkVersion conn si = when (ServiceInfo.version si < requiredServiceVersion) do
        send conn [Frame.Exception $ "Expected service version " <> show requiredServiceVersion]
        throwIO $ FatalError "It looks like you're running a development version of hercules-ci-agent that is not yet supported on hercules-ci.com. Please use the stable branch or a tag."
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
            Frame.Exception e -> logLocM WarningS $ "Service exception: " <> toSL e
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
    $ withConnection' base token
    $ \conn -> do
      handshake conn
      race_ (readThread conn) (writeThread conn)

withConnection' :: (MonadUnliftIO m) => Text -> ByteString -> (Connection -> m a) -> m a
withConnection' base token m = do
  UnliftIO unlift <- askUnliftIO
  let opts = WS.defaultConnectionOptions
      headers = [("Authorization", "Bearer " <> token)]
      runSocket
        | base == "test://api" = runClientWith "api" 80 "/api/v1/agent-socket" opts []
        | otherwise = runSecureClientWith (toS base) 443 "/api/v1/agent-socket" opts headers
  liftIO $ runSocket $ \conn -> unlift (m conn)
