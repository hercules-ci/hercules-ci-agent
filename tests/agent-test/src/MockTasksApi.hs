{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MockTasksApi
  ( withServer,
    runEval,
    runBuild,
    ServerHandle (),
  )
where

import qualified AgentTask
import Conduit
  ( MonadResource,
    MonadThrow,
    ResourceT,
    mapC,
  )
import Control.Concurrent (newEmptyMVar)
import Control.Concurrent.STM
import qualified Control.Exception.Safe
import Data.Aeson
  ( FromJSON,
    ToJSON,
    eitherDecode,
  )
import qualified Data.ByteString as BS
import qualified Data.Conduit as Conduit
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.Tar as Tar
import Data.Conduit.Zlib (gzip)
import Data.IORef
  ( IORef,
    atomicModifyIORef,
    newIORef,
    readIORef,
  )
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import qualified DummyApi
import Hercules.API.Agent
import Hercules.API.Agent.Build (BuildAPI (..))
import qualified Hercules.API.Agent.Build.BuildEvent as BuildEvent
import qualified Hercules.API.Agent.Build.BuildTask as BuildTask
import Hercules.API.Agent.Evaluate (EvalAPI (..))
import Hercules.API.Agent.Evaluate.DerivationStatus (DerivationStatus)
import qualified Hercules.API.Agent.Evaluate.DerivationStatus as DerivationStatus
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent as EvaluateEvent
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.BuildRequest as BuildRequest
import qualified Hercules.API.Agent.Evaluate.EvaluateTask as EvaluateTask
import Hercules.API.Agent.LifeCycle (LifeCycleAPI (..))
import qualified Hercules.API.Agent.LifeCycle as LifeCycle
import qualified Hercules.API.Agent.LifeCycle.CreateAgentSession_V2 as CreateAgentSession
import Hercules.API.Agent.Tasks (TasksAPI (..))
import Hercules.API.Id
import Hercules.API.Logs (LogsAPI (..))
import Hercules.API.Task (Task)
import qualified Hercules.API.Task as Task
import qualified Hercules.API.TaskStatus as TaskStatus
import Network.Wai.Handler.Warp (run)
import Orphans ()
import Protolude
import Servant.API
import Servant.API.Generic
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
import qualified Prelude

data ServerState
  = ServerState
      { queue :: MVar (Task Task.Any),
        done :: TVar (Map Text TaskStatus.TaskStatus),
        evalTasks ::
          IORef
            ( Map (Id (Task EvaluateTask.EvaluateTask))
                EvaluateTask.EvaluateTask
            ),
        evalEvents ::
          IORef
            ( Map (Id (Task EvaluateTask.EvaluateTask))
                [EvaluateEvent.EvaluateEvent]
            ),
        buildTasks ::
          IORef
            ( Map (Id (Task BuildTask.BuildTask))
                BuildTask.BuildTask
            ),
        buildEvents ::
          IORef
            ( Map (Id (Task BuildTask.BuildTask))
                [BuildEvent.BuildEvent]
            ),
        drvTasks :: IORef (Map Text (Id (Task BuildTask.BuildTask)))
      }

newtype ServerHandle = ServerHandle ServerState

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
  putMVar (queue st) (toTask t')

toTask :: AgentTask.AgentTask -> Task.Task Task.Any
toTask (AgentTask.Evaluate t) =
  Task.upcast $ Task.Task {Task.id = EvaluateTask.id t, Task.typ = "eval"}
toTask (AgentTask.Build t) =
  Task.upcast $ Task.Task {Task.id = BuildTask.id t, Task.typ = "build"}

fixup :: AgentTask.AgentTask -> IO AgentTask.AgentTask
fixup (AgentTask.Evaluate t)
  | "/" `T.isPrefixOf` EvaluateTask.primaryInput t =
    do
      env <- getEnvironment
      let base = fromMaybe "http://api" $ L.lookup "BASE_URL" env
      pure $
        AgentTask.Evaluate
          t
            { EvaluateTask.primaryInput = toS base <> (EvaluateTask.primaryInput t),
              EvaluateTask.otherInputs =
                fmap
                  (toS base <>)
                  (EvaluateTask.otherInputs t)
            }
fixup x = pure x

await :: ServerHandle -> Text -> IO TaskStatus.TaskStatus
await (ServerHandle st) t = atomically $ do
  d <- readTVar (done st)
  case M.lookup t d of
    Just s -> pure s
    Nothing -> retry

runEval ::
  ServerHandle ->
  EvaluateTask.EvaluateTask ->
  IO (TaskStatus.TaskStatus, [EvaluateEvent.EvaluateEvent])
runEval sh@(ServerHandle st) task = do
  enqueue sh (AgentTask.Evaluate task)
  s <- await sh (idText $ EvaluateTask.id task)
  evs <- readIORef (evalEvents st)
  pure $ (,) s $ fromMaybe [] $ M.lookup (EvaluateTask.id task) evs

runBuild ::
  ServerHandle ->
  BuildTask.BuildTask ->
  IO (TaskStatus.TaskStatus, [BuildEvent.BuildEvent])
runBuild sh@(ServerHandle st) task = do
  enqueue sh (AgentTask.Build task)
  s <- await sh (idText $ BuildTask.id task)
  evs <- readIORef (buildEvents st)
  pure $ (,) s $ fromMaybe [] $ M.lookup (BuildTask.id task) evs

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
  let st = ServerState
        { queue = q,
          evalEvents = ee,
          evalTasks = et,
          buildEvents = be,
          buildTasks = bt,
          drvTasks = dt,
          done = d
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
          $ run port (app st)
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
    :<|> "tarball"
    :> Capture "tarball" Text
    :> StreamGet NoFraming OctetStream (SourceIO ByteString)

mockApi :: Proxy MockAPI
mockApi = Proxy

context :: Context '[JWTSettings, CookieSettings]
context = jwtSettings :. cookieSettings :. EmptyContext

jwtSettings :: JWTSettings
jwtSettings =
  defaultJWTSettings
    $ fromRight'
    $ eitherDecode
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
    :<|> (toSourceIO & liftA & liftA & ($ sourceball))

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
  Monad m =>
  FilePath ->
  Conduit.ConduitM
    (Either Tar.FileInfo ByteString)
    (Either Tar.FileInfo ByteString)
    m
    ()
makeRelative fp =
  mapC
    ( \c -> case c of
        Left fi -> Left $ f fi
        x -> x
    )
  where
    f fi = fi {Tar.filePath = fixEmpty $ doIt $ Tar.filePath fi}
    doIt :: ByteString -> ByteString
    doIt path =
      BS.dropWhile (== fromIntegral (ord '/')) $ fromMaybe path $
        BS.stripPrefix
          (toS fp)
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
  cfname <- liftIO $ canonicalizePath $ toS ("testdata" </> toS fname)
  isFile <- liftIO $ doesFileExist cfname
  if isFile
    then pure $ Conduit.sourceFile cfname
    else
      pure
        ( Conduit.yield ("testdata" </> toS cfname)
            .| relativePathConduit
            .| void Tar.tar
            .| gzip
        )

handleTasksReady ::
  ServerState ->
  AuthResult Session ->
  Handler (Maybe (Task Task.Any))
handleTasksReady st _authResult = liftIO $ tryTakeMVar (queue st)

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
    { tasksReady = handleTasksReady server,
      tasksSetStatus = handleTasksSetStatus server
    }

handleTasksUpdate ::
  ServerState ->
  Id (Task EvaluateTask.EvaluateTask) ->
  [EvaluateEvent.EvaluateEvent] ->
  AuthResult Session ->
  Handler NoContent
handleTasksUpdate st id body _authResult = do
  liftIO $ atomicModifyIORef_ (evalEvents st) $ \m ->
    M.alter (\prev -> Just $ fromMaybe mempty prev <> body) id m
  for_ body $ \ev -> case ev of
    EvaluateEvent.BuildRequest (BuildRequest.BuildRequest {derivationPath = drvPath}) -> do
      buildId <- liftIO randomId
      liftIO $ enqueue (ServerHandle st) $ AgentTask.Build $ BuildTask.BuildTask
        { BuildTask.id = buildId,
          BuildTask.derivationPath = drvPath,
          BuildTask.logToken = "eyBlurb="
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
    Nothing -> pure $ Just (uuid, DerivationStatus.BuildFailure) -- TODO exception failure
    Just taskId -> do
      dones <- liftIO $ atomically $ readTVar (done server)
      pure ((\s -> (uuid, translate s)) <$> M.lookup (idText taskId) dones)
  where
    translate TaskStatus.Exceptional {} = DerivationStatus.BuildFailure
    translate TaskStatus.Terminated {} = DerivationStatus.BuildFailure
    translate TaskStatus.Successful {} = DerivationStatus.BuildSuccess

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ r = atomicModifyIORef r . ((,()) .)

data Session = Session (Id "AgentSession")
  deriving (Generic, ToJSON, FromJSON)

instance FromJWT Session

instance ToJWT Session

type Auth' = Auth '[JWT] Session

lifeCycleEndpoints :: ServerState -> LifeCycleAPI Auth' AsServer
lifeCycleEndpoints _server =
  DummyApi.dummyLifeCycleEndpoints
    { LifeCycle.agentSessionCreate = handleAgentCreate,
      LifeCycle.hello = \_ _ -> pure NoContent,
      LifeCycle.heartbeat = \_ _ -> pure NoContent,
      LifeCycle.goodbye = \_ _ -> pure NoContent
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
  liftIO $ atomicModifyIORef_ (buildEvents st) $ \m ->
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
logsEndpoints _server = LogsAPI
  { writeLog = \_authResult logBytes -> NoContent <$ do
      hPutStrLn stderr $ "Got log: " <> logBytes
  }

randomId :: IO (Id a)
randomId = Id <$> UUID.nextRandom
