{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module MockTasksApi
  ( withServer
  , runEval
  , ServerHandle()
  )
where

import           Protolude
import           Servant.Server
import           Network.Wai.Handler.Warp       ( run )
import           Servant.API
import           Servant.API.Generic
import           Servant.Auth.Server
import           Servant.Server.Generic
import           Servant.Streaming.Server
import           Conduit                        ( ResourceT
                                                , MonadThrow
                                                , MonadResource
                                                , mapC
                                                )
import           Control.Monad.Morph            ( hoist )
import           Data.Aeson                     ( eitherDecode
                                                , ToJSON
                                                , FromJSON
                                                )
import qualified Data.ByteString               as BS
import qualified Data.Conduit                  as Conduit
import           Data.Conduit                   ( (.|) )
import qualified Data.Conduit.Combinators      as Conduit
import           Data.IORef                     ( IORef
                                                , newIORef
                                                , readIORef
                                                , atomicModifyIORef
                                                )
import qualified Data.List                     as L
import qualified Data.Map                      as M
import           Hercules.API
import           Hercules.API.Id
import           Hercules.API.Agents            ( AgentsAPI )
import qualified Hercules.API.Agents           as API.Agents
import qualified Hercules.API.Agents.CreateAgentSession
                                               as CreateAgentSession
import           Hercules.API.Agents.AgentSession      ( AgentSession )
import qualified Hercules.API.AgentTask        as AgentTask
import           Hercules.API.Task              ( Task )
import qualified Hercules.API.Task             as Task
import qualified Hercules.API.TaskStatus       as TaskStatus
import qualified Hercules.API.EvaluateTask     as EvaluateTask
import qualified Hercules.API.EvaluateEvent    as EvaluateEvent
import           Control.Concurrent             ( newEmptyMVar )
import           Control.Concurrent.STM
import qualified Streaming                     as Streaming
import qualified Streaming.Prelude             as Streaming
import           System.Environment             ( getEnvironment )
import           System.Directory               ( canonicalizePath
                                                , doesFileExist
                                                )
import           System.FilePath                ( (</>) )
import qualified Prelude
import qualified Data.Text                     as T
import qualified Data.Conduit.Tar              as Tar
import           Data.Conduit.Zlib              ( gzip )
import qualified DummyApi
import           Orphans                        ()

data ServerState = ServerState
  { queue :: MVar (Task Task.Any)
  , evalTasks :: IORef (Map (Id (Task EvaluateTask.EvaluateTask))
                            EvaluateTask.EvaluateTask)
  , evalEvents :: IORef (Map (Id (Task EvaluateTask.EvaluateTask))
                             [EvaluateEvent.EvaluateEvent])
  , done :: TVar (Map Text TaskStatus.TaskStatus)
  }

newtype ServerHandle = ServerHandle ServerState

enqueue :: ServerHandle -> AgentTask.AgentTask -> IO ()
enqueue (ServerHandle st) t = do
  t' <- fixup t

  case t' of
    AgentTask.Evaluate evalTask -> do
      atomicModifyIORef_ (evalTasks st)
        $ M.insert (EvaluateTask.id evalTask) evalTask
    _ -> panic "non-evaluate task not supported"
  putMVar (queue st) (toTask t')

toTask :: AgentTask.AgentTask -> Task.Task Task.Any
toTask (AgentTask.Evaluate t) =
  Task.upcast $ Task.Task {Task.id = EvaluateTask.id t, Task.typ = "eval"}
toTask _ = panic "unsupported task type in MockTasksApi.toTask"

fixup :: AgentTask.AgentTask -> IO AgentTask.AgentTask
fixup (AgentTask.Evaluate t) | "/" `T.isPrefixOf` EvaluateTask.primaryInput t =
  do
    env <- getEnvironment
    let base = fromMaybe "http://api" $ L.lookup "BASE_URL" env
    pure $ AgentTask.Evaluate t
      { EvaluateTask.primaryInput = toS base <> (EvaluateTask.primaryInput t)
      , EvaluateTask.otherInputs = fmap (toS base <>)
                                        (EvaluateTask.otherInputs t)
      }
fixup x = pure x

await :: ServerHandle -> Text -> IO TaskStatus.TaskStatus
await (ServerHandle st) t = atomically $ do
  d <- readTVar (done st)
  case M.lookup t d of
    Just s -> pure s
    Nothing -> retry

runEval :: ServerHandle
        -> EvaluateTask.EvaluateTask
        -> IO (TaskStatus.TaskStatus, [EvaluateEvent.EvaluateEvent])
runEval sh@(ServerHandle st) task = do
  enqueue sh (AgentTask.Evaluate task)
  s <- await sh (idText $ EvaluateTask.id task)
  evs <- readIORef (evalEvents st)
  pure $ (,) s $ fromMaybe [] $ M.lookup (EvaluateTask.id task) evs

withServer :: (ServerHandle -> IO ()) -> IO ()
withServer doIt = do

  env <- getEnvironment
  let port = maybe 80 Prelude.read $ L.lookup "PORT" env

  q <- newEmptyMVar
  ee <- newIORef mempty
  et <- newIORef mempty
  d <- newTVarIO mempty
  let st = ServerState {queue = q, evalEvents = ee, evalTasks = et, done = d}
      runServer =
        handle
            (\e -> do -- TODO: ignore asynccanceled
              putText
                $ "Exception in mock server at port "
                <> show port
                <> ": "
                <> show (e :: SomeException)
              throwIO e
            )
          $ run port (app st)

  withAsync runServer $ \a -> do
    doIt $ ServerHandle st
    cancel a

type MockAPI = AddAPIVersion ( ToServantApi (TasksAPI Auth')
                             :<|> ToServantApi (EvalAPI Auth')
                             :<|> ToServantApi (AgentsAPI Auth')
                             )
          :<|> "tarball" :> Capture "tarball" Text :> StreamResponseGet '[OctetStream]

mockApi :: Proxy MockAPI
mockApi = Proxy

context :: Context '[JWTSettings, CookieSettings]
context = jwtSettings :. cookieSettings :. EmptyContext

jwtSettings :: JWTSettings
jwtSettings =
  defaultJWTSettings
    $ fromRight
    $ eitherDecode
        "{\"crv\":\"P-256\",\"d\":\"BWOmuvMiIPUWR-sPHIxaEKKr59OlVj-C7j24sgtCqA0\",\"x\":\"TTmrmU8p4PO3JGuW-8Fc2EvCBoR5NVoT2N5J3wJzBHg\",\"kty\":\"EC\",\"y\":\"6ATtNfAzjk_I4qf2hDrf2kAOw9IFZK8Y2ECJcs_fjqM\"}"
 where
  fromRight (Right r) = r
  fromRight (Left l) = panic $ "test suite static jwk decode error" <> show l

cookieSettings :: CookieSettings
cookieSettings = defaultCookieSettings

app :: ServerState -> Application
app server = serveWithContext mockApi context $ endpoints server

endpoints :: ServerState -> Server MockAPI
endpoints server =
  (toServant (taskEndpoints server)
    :<|> toServant (evalEndpoints server)
    :<|> toServant (agentsEndpoints server)
    )
    :<|> sourceball

instance Streaming.MFunctor (Conduit.ConduitT a b) where
  hoist = Conduit.transPipe

relativePathConduit :: (MonadThrow m, MonadResource m)
                    => Conduit.ConduitM
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
makeRelative :: Monad m
             => FilePath
             -> Conduit.ConduitM
                  (Either Tar.FileInfo ByteString)
                  (Either Tar.FileInfo ByteString)
                  m
                  ()
makeRelative fp = mapC
  (\c -> case c of
    Left fi -> Left $ f fi
    x -> x
  )
 where
  f fi = fi { Tar.filePath = fixEmpty $ doIt $ Tar.filePath fi }
  doIt :: ByteString -> ByteString
  doIt path =
    BS.dropWhile (== fromIntegral (ord '/')) $ fromMaybe path $ BS.stripPrefix
      (toS fp)
      path
  fixEmpty :: ByteString -> ByteString
  fixEmpty "" = "."
  fixEmpty x = x

sourceball :: Text
           -> Handler
                ( Streaming.Stream
                    (Streaming.Of ByteString)
                    (ResourceT IO)
                    ()
                )
sourceball "broken-tarball" = do
  pure $ streamConduit (Conduit.sourceLazy "i'm not a tarball")
sourceball fname = do
  cfname <- liftIO $ canonicalizePath $ toS ("testdata" </> toS fname)
  isFile <- liftIO $ doesFileExist cfname
  if isFile
    then pure $ streamConduit $ Conduit.sourceFile cfname
    else pure $ streamConduit
      (Conduit.yield ("testdata" </> toS cfname)
      .| relativePathConduit
      .| void Tar.tar
      .| gzip
      )

streamConduit :: Conduit.ConduitT () o (ResourceT IO) ()
              -> (Streaming.Stream (Streaming.Of o) (ResourceT IO) ())
streamConduit conduit =
  Conduit.runConduit $ hoist lift (conduit) .| Conduit.mapM_ (Streaming.yield)


handleTasksReady :: ServerState
                 -> AuthResult Session
                 -> Handler (Maybe (Task Task.Any))
handleTasksReady st authResult = liftIO $ tryTakeMVar (queue st)

handleTasksSetStatus :: ServerState
                     -> Id (Task Task.Any)
                     -> TaskStatus.TaskStatus
                     -> AuthResult Session
                     -> Handler NoContent
handleTasksSetStatus st tid status authResult = do
  liftIO $ atomically $ modifyTVar (done st) (M.insert (idText tid) status) -- FIXME: check for double setStatus
  pure NoContent

taskEndpoints :: ServerState -> TasksAPI Auth' AsServer
taskEndpoints server = DummyApi.dummyTasksEndpoints
  { tasksReady = handleTasksReady server
  , tasksSetStatus = handleTasksSetStatus server
  }

handleTasksUpdate :: ServerState
                  -> Id (Task EvaluateTask.EvaluateTask)
                  -> [EvaluateEvent.EvaluateEvent]
                  -> AuthResult Session
                  -> Handler NoContent
handleTasksUpdate st id body authResult = do
  liftIO $ atomicModifyIORef_ (evalEvents st) $ \m ->
    M.alter (\prev -> Just $ fromMaybe mempty prev <> body) id m

  pure NoContent

handleTasksGetEvaluation :: ServerState
                         -> Id (Task EvaluateTask.EvaluateTask)
                         -> AuthResult Session
                         -> Handler EvaluateTask.EvaluateTask
handleTasksGetEvaluation st id authResult = do
  ts <- liftIO $ readIORef (evalTasks st)
  case M.lookup id ts of
    Nothing -> throwError err404
    Just x -> pure x

evalEndpoints :: ServerState -> EvalAPI Auth' AsServer
evalEndpoints server = DummyApi.dummyEvalEndpoints
  { tasksGetEvaluation = handleTasksGetEvaluation server
  , tasksUpdateEvaluation = handleTasksUpdate server
  }

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ r = atomicModifyIORef r . ((, ()) .)

data Session = Session (Id AgentSession)
  deriving (Generic, ToJSON, FromJSON)
instance FromJWT Session
instance ToJWT Session

type Auth' = Auth '[JWT] Session

agentsEndpoints :: ServerState -> AgentsAPI Auth' AsServer
agentsEndpoints server =
  DummyApi.dummyAgentsEndpoints { API.Agents.agentSessionCreate = handleAgentCreate }

handleAgentCreate :: CreateAgentSession.CreateAgentSession
                  -> AuthResult Session
                  -> Handler Text
handleAgentCreate ca r = do
  pure "pretend-jwt"
