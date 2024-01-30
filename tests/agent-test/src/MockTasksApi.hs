{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MockTasksApi
  ( withServer,
    runEval,
    runBuild,
    ServerHandle (),
    serverState,
    logEntries,
    fixupInputs,
    runEffect,
  )
where

import AgentTask qualified
import Conduit
  ( MonadResource,
    MonadThrow,
    ResourceT,
    mapC,
  )
import Control.Concurrent.STM
import Control.Exception.Safe qualified
import Data.Aeson
  ( FromJSON,
    ToJSON,
    eitherDecode,
  )
import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Conduit ((.|))
import Data.Conduit qualified as Conduit
import Data.Conduit.Combinators qualified as Conduit
import Data.Conduit.Tar qualified as Tar
import Data.Conduit.Zlib (gzip)
import Data.IORef
  ( IORef,
    atomicModifyIORef,
    modifyIORef,
    newIORef,
    readIORef,
  )
import Data.List qualified as L
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import DummyApi qualified
import Hercules.API.Agent
import Hercules.API.Agent.Build (BuildAPI (..))
import Hercules.API.Agent.Build.BuildEvent qualified as BuildEvent
import Hercules.API.Agent.Build.BuildTask qualified as BuildTask
import Hercules.API.Agent.Effect.EffectTask qualified as EffectTask
import Hercules.API.Agent.Evaluate (EvalAPI (..))
import Hercules.API.Agent.Evaluate.DerivationStatus (DerivationStatus)
import Hercules.API.Agent.Evaluate.DerivationStatus qualified as DerivationStatus
import Hercules.API.Agent.Evaluate.EvaluateEvent qualified as EvaluateEvent
import Hercules.API.Agent.Evaluate.EvaluateEvent.BuildRequest qualified as BuildRequest
import Hercules.API.Agent.Evaluate.EvaluateEvent.BuildRequired qualified as BuildRequired
import Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo (DerivationInfo)
import Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo qualified as DerivationInfo
import Hercules.API.Agent.Evaluate.EvaluateTask qualified as EvaluateTask
import Hercules.API.Agent.Evaluate.ImmutableInput qualified as ImmutableInput
import Hercules.API.Agent.LifeCycle (LifeCycleAPI (..))
import Hercules.API.Agent.LifeCycle qualified as LifeCycle
import Hercules.API.Agent.LifeCycle.CreateAgentSession_V2 qualified as CreateAgentSession
import Hercules.API.Agent.LifeCycle.ServiceInfo qualified as SI
import Hercules.API.Agent.Socket.AgentPayload (AgentPayload)
import Hercules.API.Agent.Socket.Frame as Frame
import Hercules.API.Agent.Socket.ServicePayload (ServicePayload)
import Hercules.API.Agent.Socket.ServicePayload qualified as SP
import Hercules.API.Agent.Tasks (TasksAPI (..))
import Hercules.API.Id
import Hercules.API.Logs (LogsAPI (..))
import Hercules.API.Logs.LogEntry (LogEntry)
import Hercules.API.Logs.LogMessage (LogMessage (LogEntries))
import Hercules.API.Task (Task)
import Hercules.API.Task qualified as Task
import Hercules.API.TaskStatus qualified as TaskStatus
import Network.Wai.Handler.Warp (run)
import Network.WebSockets qualified as WS
import Network.WebSockets.Connection qualified
import Orphans ()
import Protolude hiding (Handler)
import Servant.API
import Servant.API.WebSocket
import Servant.Auth.Server
import Servant.Conduit ()
import Servant.Server
import Servant.Server.Generic
import System.Directory
  ( canonicalizePath,
    doesFileExist,
  )
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import TestSupport
import Prelude qualified

data ServerState = ServerState
  { queue :: MVar ServicePayload,
    done :: TVar (Map Text TaskStatus.TaskStatus),
    evalTasks ::
      IORef
        ( Map
            (Id (Task EvaluateTask.EvaluateTask))
            EvaluateTask.EvaluateTask
        ),
    evalEvents ::
      IORef
        ( Map
            (Id (Task EvaluateTask.EvaluateTask))
            [EvaluateEvent.EvaluateEvent]
        ),
    derivationInfos ::
      IORef (Map Text DerivationInfo),
    ifdDerivations :: IORef (Set Text),
    buildTasks ::
      IORef
        ( Map
            (Id (Task BuildTask.BuildTask))
            BuildTask.BuildTask
        ),
    buildEvents ::
      IORef
        ( Map
            (Id (Task BuildTask.BuildTask))
            [BuildEvent.BuildEvent]
        ),
    drvTasks :: IORef (Map Text (Id (Task BuildTask.BuildTask))),
    effectTasks :: IORef (Map Text (Id (Task EffectTask.EffectTask))),
    logEntries :: TVar [LogEntry]
  }

newtype ServerHandle = ServerHandle ServerState

serverState :: ServerHandle -> ServerState
serverState (ServerHandle st) = st

enqueue :: ServerHandle -> AgentTask.AgentTask -> IO ()
enqueue (ServerHandle st) t = do
  t' <- fixup t
  case t' of
    AgentTask.Evaluate evalTask ->
      atomicModifyIORef_ (evalTasks st) $
        M.insert (EvaluateTask.id evalTask) evalTask
    AgentTask.Build buildTask -> do
      atomicModifyIORef_ (buildTasks st) $
        M.insert (BuildTask.id buildTask) buildTask
      atomicModifyIORef_ (drvTasks st) $
        M.insert (BuildTask.derivationPath buildTask) (BuildTask.id buildTask)
    AgentTask.Effect effectTask -> do
      atomicModifyIORef_ (effectTasks st) $
        M.insert (EffectTask.derivationPath effectTask) (EffectTask.id effectTask)

  putMVar (queue st) (toServicePayload t')

toServicePayload :: AgentTask.AgentTask -> SP.ServicePayload
toServicePayload = \case
  AgentTask.Evaluate t -> SP.StartEvaluation t
  AgentTask.Build t -> SP.StartBuild t
  AgentTask.Effect t -> SP.StartEffect t

fixup :: AgentTask.AgentTask -> IO AgentTask.AgentTask
fixup (AgentTask.Evaluate t) = do
  let base = apiBaseUrl
      otherInputs' =
        t & EvaluateTask.otherInputs & map \input ->
          if "/" `T.isPrefixOf` input
            then base <> input
            else input
  pure $
    AgentTask.Evaluate
      t
        { EvaluateTask.otherInputs = otherInputs'
        }
fixup x = pure x

await :: ServerHandle -> Text -> IO TaskStatus.TaskStatus
await (ServerHandle st) t = atomically $ do
  d <- readTVar (done st)
  maybe retry pure (M.lookup t d)

runEval ::
  ServerHandle ->
  EvaluateTask.EvaluateTask ->
  IO (TaskStatus.TaskStatus, [EvaluateEvent.EvaluateEvent])
runEval sh@(ServerHandle st) task = do
  enqueue sh (AgentTask.Evaluate task)
  s <- await sh (idText $ EvaluateTask.id task)
  evs <- readIORef (evalEvents st)
  pure $ (,) s $ fromMaybe [] $ M.lookup (EvaluateTask.id task) evs

fixupInputs :: EvaluateTask.EvaluateTask -> EvaluateTask.EvaluateTask
fixupInputs t = t {EvaluateTask.inputs = ImmutableInput.ArchiveUrl <$> EvaluateTask.otherInputs t}

runBuild ::
  ServerHandle ->
  BuildTask.BuildTask ->
  IO (TaskStatus.TaskStatus, [BuildEvent.BuildEvent])
runBuild sh@(ServerHandle st) task0 = do
  task <- fixupBuildTask sh task0
  enqueue sh (AgentTask.Build task)
  s <- await sh (idText $ BuildTask.id task)
  evs <- readIORef (buildEvents st)
  pure $ (,) s $ fromMaybe [] $ M.lookup (BuildTask.id task) evs

runEffect ::
  ServerHandle ->
  EffectTask.EffectTask ->
  IO (TaskStatus.TaskStatus)
runEffect sh task0 = do
  task <- fixupEffectTask sh task0
  enqueue sh (AgentTask.Effect task)
  await sh (idText $ EffectTask.id task)

getInOuts :: ServerHandle -> Text -> IO [Text]
getInOuts (ServerHandle st) drvPath = do
  drvs <- readIORef $ derivationInfos st
  pure
    [ outPath
      | di <- toList $ drvs & M.lookup drvPath,
        (dep, outs) <- DerivationInfo.inputDerivations di & M.toList,
        depInfo <- toList $ drvs & M.lookup dep,
        out <- outs,
        outInfo <- depInfo & DerivationInfo.outputs & M.lookup out & toList,
        outPath <- DerivationInfo.path outInfo & toList
    ]

fixupEffectTask :: ServerHandle -> EffectTask.EffectTask -> IO EffectTask.EffectTask
fixupEffectTask s t = do
  inouts <- getInOuts s (EffectTask.derivationPath t)
  pure $ t {EffectTask.inputDerivationOutputPaths = inouts}

fixupBuildTask :: ServerHandle -> BuildTask.BuildTask -> IO BuildTask.BuildTask
fixupBuildTask s t = do
  inouts <- getInOuts s (BuildTask.derivationPath t)
  pure $ t {BuildTask.inputDerivationOutputPaths = inouts}

printState :: ServerState -> IO ()
printState st = do
  putErrText "-----"
  putErrText "Mock Server Diagnostic"
  putErrText "-----"
  putErrText . ("queue: " <>) . show <=< tryReadMVar $ st.queue
  done <- readTVarIO st.done
  for_ (M.toList done) \(key, value) -> do
    putErrText ("task " <> show key <> ": " <> show value)
  putErrText "-----"

withServer :: (ServerHandle -> IO ()) -> IO ()
withServer doIt = do
  env <- getEnvironment
  let port = maybe 80 Prelude.read $ L.lookup "PORT" env
  q <- newEmptyMVar
  ee <- newIORef mempty
  et <- newIORef mempty
  be <- newIORef mempty
  bt <- newIORef mempty
  dt <- newIORef mempty
  d <- newTVarIO mempty
  le <- newTVarIO mempty
  dis <- newIORef mempty
  ifdDrvs <- newIORef mempty
  eft <- newIORef mempty
  let st =
        ServerState
          { queue = q,
            evalEvents = ee,
            derivationInfos = dis,
            evalTasks = et,
            buildEvents = be,
            buildTasks = bt,
            drvTasks = dt,
            done = d,
            logEntries = le,
            ifdDerivations = ifdDrvs,
            effectTasks = eft
          }
      runServer =
        Control.Exception.Safe.handleAny
          ( \e -> do
              putText $
                "Exception in mock server at port "
                  <> show port
                  <> ": "
                  <> show (e :: SomeException)
              throwIO e
          )
          $ do
            _ <- forkIO do
              forever do
                threadDelay (60 * 1000 * 1000)
                printState st
            run port (app st)
  withAsync runServer $ \a -> do
    doIt $ ServerHandle st
    cancel a

type MockAPI =
  AddAPIVersion
    ( ToServantApi (TasksAPI Auth')
        :<|> ToServantApi (EvalAPI Auth')
        :<|> ToServantApi (BuildAPI Auth')
        :<|> ToServantApi (LogsAPI Session)
        :<|> ToServantApi (LifeCycleAPI Auth')
    )
    :<|> "api" :> "v1" :> "agent-socket" :> WebSocket
    :<|> "api" :> "v1" :> "logs" :> "build" :> "socket" :> WebSocket
    :<|> "tarball" :> Capture "tarball" Text :> StreamGet NoFraming OctetStream (SourceIO ByteString)
    :<|> "hello" :> Get '[PlainText] Text

mockApi :: Proxy MockAPI
mockApi = Proxy

context :: Context '[JWTSettings, CookieSettings]
context = jwtSettings :. cookieSettings :. EmptyContext

jwtSettings :: JWTSettings
jwtSettings =
  defaultJWTSettings $
    fromRight' $
      eitherDecode
        "{\"crv\":\"P-256\",\"d\":\"BWOmuvMiIPUWR-sPHIxaEKKr59OlVj-C7j24sgtCqA0\",\"x\":\"TTmrmU8p4PO3JGuW-8Fc2EvCBoR5NVoT2N5J3wJzBHg\",\"kty\":\"EC\",\"y\":\"6ATtNfAzjk_I4qf2hDrf2kAOw9IFZK8Y2ECJcs_fjqM\"}"
  where
    fromRight' (Right r) = r
    fromRight' (Left l) = panic $ "test suite static jwk decode error" <> show l

cookieSettings :: CookieSettings
cookieSettings = defaultCookieSettings

app :: ServerState -> Application
app server = serveWithContext mockApi context $ endpoints server

endpoints :: ServerState -> Server MockAPI
endpoints server =
  ( toServant (taskEndpoints server)
      :<|> toServant (evalEndpoints server)
      :<|> toServant (buildEndpoints server)
      :<|> toServant (logsEndpoints server)
      :<|> toServant (lifeCycleEndpoints server)
  )
    :<|> socket server
    :<|> logSocket server
    :<|> (toSourceIO & liftA & liftA & ($ sourceball))
    :<|> pure "Hello, world! - The fake API testing endpoint\n"

logSocket :: ServerState -> Network.WebSockets.Connection.Connection -> Handler ()
logSocket server conn = do
  let send :: (MonadIO m) => [Frame SI.ServiceInfo SI.ServiceInfo] -> m ()
      send = send' conn
      -- FIXME
      recv :: Handler (Frame LogMessage LogMessage)
      recv = recv' conn
  si <- liftIO serviceInfo
  send [Frame.Oob {o = si}]
  _hello <- recv
  send [Frame.Ack (-1)]
  forever $ do
    pl <- recv
    case pl of
      Frame.Exception {message = msg} -> panic $ "Agent exception: " <> msg
      Frame.Ack {} -> pass
      Frame.Oob {} -> pass
      Frame.Msg {p = payload, n = number} -> do
        processLogPayload server payload
        send [Frame.Ack number]

processLogPayload :: ServerState -> LogMessage -> Handler ()
processLogPayload server (LogEntries les) = do
  liftIO $ atomically do
    modifyTVar (logEntries server) (toList les ++)
processLogPayload _ _ =
  pass

socket :: ServerState -> Network.WebSockets.Connection.Connection -> Handler ()
socket server conn = do
  let send :: (MonadIO m) => [Frame ServicePayload ServicePayload] -> m ()
      send = send' conn
      recv :: (MonadIO m) => m (Frame AgentPayload AgentPayload)
      recv = recv' conn
  si <- liftIO serviceInfo
  send [Frame.Oob {o = SP.ServiceInfo si}]
  _hello <- recv
  send [Frame.Ack (-1)]
  _writerThreadId <- liftIO $
    flip forkFinally (putErrText . ("Writer died: " <>) . show @(Either SomeException Void)) $ do
      let doSend msgN = do
            payload <- takeMVar (queue server)
            send [Frame.Msg {p = payload, n = msgN}]
            doSend (msgN + 1)
      doSend 0
  forever $ do
    pl <- recv
    case pl of
      Frame.Exception {message = msg} -> panic $ "Agent exception: " <> msg
      Frame.Ack {} -> pass
      Frame.Oob {} -> pass
      Frame.Msg {p = payload, n = number} -> do
        processPayload payload
        send [Frame.Ack number]

processPayload :: AgentPayload -> Handler ()
processPayload _ap = pass

send' :: forall sp m. (ToJSON sp, MonadIO m) => WS.Connection -> [Frame sp sp] -> m ()
send' conn msgs = do
  let bs = A.encode <$> msgs
  forM_ bs $ putErrText . ("Service message: " <>) . decodeUtf8With lenientDecode . BL.toStrict
  liftIO $ WS.sendDataMessages conn (WS.Binary <$> bs)

recv' :: forall ap m. (FromJSON ap, Show ap, MonadIO m) => WS.Connection -> m (Frame ap ap)
recv' conn = do
  liftIO (A.eitherDecode <$> WS.receiveData conn) >>= \case
    Left e -> liftIO $ throwIO (FatalError $ "Error decoding agent message: " <> toS e)
    Right (Frame.Exception e) -> do
      putErrText $ "Agent exception message: " <> toS e
      recv' conn
    Right r -> do
      putErrText $ "Agent message: " <> toS (show r :: Text)
      pure r

relativePathConduit ::
  (MonadThrow m, MonadResource m) =>
  Conduit.ConduitM
    FilePath
    (Either Tar.FileInfo ByteString)
    m
    ()
relativePathConduit = do
  mfp <- Conduit.await
  forM_ mfp $ \fp -> Conduit.yield fp .| Tar.filePathConduit .| makeRelative fp

-- | DON'T USE THIS IN PRODUCTION.
--   - This performs comparisions on strings that have been converted back
--     and forth
--   - If it fails the absolute path WILL be used. This will break your app
--     leak information about your paths!
makeRelative ::
  (Monad m) =>
  FilePath ->
  Conduit.ConduitM
    (Either Tar.FileInfo ByteString)
    (Either Tar.FileInfo ByteString)
    m
    ()
makeRelative fp =
  mapC
    ( \case
        Left fi -> Left $ f fi
        x -> x
    )
  where
    f fi = fi {Tar.filePath = fixEmpty $ doIt $ Tar.filePath fi}
    doIt :: ByteString -> ByteString
    doIt path =
      BS.dropWhile (== fromIntegral (ord '/')) $
        fromMaybe path $
          BS.stripPrefix
            (encodeUtf8 $ toS fp)
            path
    fixEmpty :: ByteString -> ByteString
    fixEmpty "" = "."
    fixEmpty x = x

sourceball ::
  Text ->
  Handler
    ( Conduit.ConduitT i ByteString (ResourceT IO) ()
    )
sourceball "broken-tarball" = pure (Conduit.sourceLazy "i'm not a tarball")
sourceball fname = do
  env <- liftIO getEnvironment
  let testdata = fromMaybe "testdata" $ L.lookup "TESTDATA" env
  cfname <- liftIO $ canonicalizePath $ toS (testdata </> toS fname)
  isFile <- liftIO $ doesFileExist cfname
  if isFile
    then pure $ Conduit.sourceFile cfname
    else
      pure
        ( Conduit.yield (testdata </> toS cfname)
            .| relativePathConduit
            .| void Tar.tar
            .| gzip
        )

handleTasksSetStatus ::
  ServerState ->
  Id (Task Task.Any) ->
  TaskStatus.TaskStatus ->
  AuthResult Session ->
  Handler NoContent
handleTasksSetStatus st tid status _authResult = do
  liftIO $ atomically $ modifyTVar (done st) (M.insert (idText tid) status) -- FIXME: check for double setStatus
  pure NoContent

taskEndpoints :: ServerState -> TasksAPI Auth' AsServer
taskEndpoints server =
  DummyApi.dummyTasksEndpoints
    { tasksSetStatus = handleTasksSetStatus server
    }

handleTasksUpdate ::
  ServerState ->
  Id (Task EvaluateTask.EvaluateTask) ->
  [EvaluateEvent.EvaluateEvent] ->
  AuthResult Session ->
  Handler NoContent
handleTasksUpdate st id body _authResult = do
  let newDrvInfos = [(DerivationInfo.derivationPath di, di) | EvaluateEvent.DerivationInfo di <- body]
  liftIO do
    atomicModifyIORef_ (evalEvents st) $ \m ->
      M.alter (\prev -> Just $ fromMaybe mempty prev <> body) id m
    atomicModifyIORef_ (derivationInfos st) $
      M.union (M.fromList newDrvInfos)
  for_ body $ \case
    EvaluateEvent.BuildRequired BuildRequired.BuildRequired {derivationPath = drvPath} -> liftIO do
      modifyIORef st.ifdDerivations (<> S.singleton drvPath)
    EvaluateEvent.BuildRequest BuildRequest.BuildRequest {derivationPath = drvPath} ->
      liftIO do
        ifds <- readIORef st.ifdDerivations
        -- TODO: Remove this `when`. Make it so that it's feasible to treat non-ifd builds realistically as well.
        --       For now doing the non-ifd builds is too much logs.
        when (S.member drvPath ifds) do
          buildId <- randomId
          enqueue (ServerHandle st)
            . AgentTask.Build
            <=< fixupBuildTask (ServerHandle st)
            $ BuildTask.BuildTask
              { BuildTask.id = buildId,
                BuildTask.derivationPath = drvPath,
                BuildTask.logToken = "eyBlurb=",
                inputDerivationOutputPaths = []
              }
    _ -> pass
  pure NoContent

handleTasksGetEvaluation ::
  ServerState ->
  Id (Task EvaluateTask.EvaluateTask) ->
  AuthResult Session ->
  Handler EvaluateTask.EvaluateTask
handleTasksGetEvaluation st id _authResult = do
  ts <- liftIO $ readIORef (evalTasks st)
  case M.lookup id ts of
    Nothing -> throwError err404
    Just x -> pure x

evalEndpoints :: ServerState -> EvalAPI Auth' AsServer
evalEndpoints server =
  DummyApi.dummyEvalEndpoints
    { tasksGetEvaluation = handleTasksGetEvaluation server,
      tasksUpdateEvaluation = handleTasksUpdate server,
      getDerivationStatus2 = handleGetDerivationStatus server
    }

handleGetDerivationStatus :: ServerState -> Text -> AuthResult Session -> Handler (Maybe (UUID, DerivationStatus))
handleGetDerivationStatus server drv _auth = do
  drvPaths <- liftIO $ readIORef (drvTasks server)
  uuid <- liftIO UUID.nextRandom
  case M.lookup drv drvPaths of
    Nothing -> pure Nothing
    Just taskId -> do
      dones <- liftIO $ atomically $ readTVar (done server)
      pure ((\s -> (uuid, translate s)) <$> M.lookup (idText taskId) dones)
  where
    translate TaskStatus.Exceptional {} = DerivationStatus.BuildFailure
    translate TaskStatus.Terminated {} = DerivationStatus.BuildFailure
    translate TaskStatus.Successful {} = DerivationStatus.BuildSuccess

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ r = atomicModifyIORef r . ((,()) .)

newtype Session = Session (Id "AgentSession")
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

instance FromJWT Session

instance ToJWT Session

type Auth' = Auth '[JWT] Session

lifeCycleEndpoints :: ServerState -> LifeCycleAPI Auth' AsServer
lifeCycleEndpoints _server =
  DummyApi.dummyLifeCycleEndpoints
    { LifeCycle.agentSessionCreate = handleAgentCreate,
      LifeCycle.hello = \_ _ -> pure NoContent,
      LifeCycle.heartbeat = \_ _ -> pure NoContent,
      LifeCycle.goodbye = \_ _ -> pure NoContent,
      LifeCycle.getServiceInfo = liftIO serviceInfo
    }

serviceInfo :: IO SI.ServiceInfo
serviceInfo = do
  pure
    SI.ServiceInfo
      { SI.version = (2, 0),
        SI.agentSocketBaseURL = apiBaseUrl,
        SI.bulkSocketBaseURL = apiBaseUrl
      }

handleAgentCreate ::
  CreateAgentSession.CreateAgentSession ->
  AuthResult Session ->
  Handler Text
handleAgentCreate _ca _r = pure "pretend-jwt"

buildEndpoints :: ServerState -> BuildAPI Auth' AsServer
buildEndpoints server =
  DummyApi.dummyBuildEndpoints
    { getBuild = handleGetBuild server,
      updateBuild = handleUpdateBuild server
    }

handleUpdateBuild ::
  ServerState ->
  Id (Task BuildTask.BuildTask) ->
  [BuildEvent.BuildEvent] ->
  AuthResult Session ->
  Handler NoContent
handleUpdateBuild st id body _authResult = do
  liftIO $
    atomicModifyIORef_ (buildEvents st) $ \m ->
      M.alter (\prev -> Just $ fromMaybe mempty prev <> body) id m
  pure NoContent

handleGetBuild ::
  ServerState ->
  Id (Task BuildTask.BuildTask) ->
  AuthResult Session ->
  Handler BuildTask.BuildTask
handleGetBuild st id _authResult = do
  ts <- liftIO $ readIORef (buildTasks st)
  case M.lookup id ts of
    Nothing -> throwError err404
    Just x -> pure x

logsEndpoints :: ServerState -> LogsAPI Session AsServer
logsEndpoints _server =
  LogsAPI
    { writeLog = \_authResult logBytes ->
        NoContent <$ do
          hPutStrLn stderr $ "Got log: " <> logBytes
    }

randomId :: IO (Id a)
randomId = Id <$> UUID.nextRandom
