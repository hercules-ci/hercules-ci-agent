{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Agent.Evaluate
  ( performEvaluation,
  )
where

import Conduit
import qualified Control.Concurrent.Async.Lifted as Async.Lifted
import Control.Concurrent.Chan.Lifted
import Control.Lens (at, (^?))
import qualified Data.Aeson as A
import Data.Aeson.Lens (_String)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Conduit.Process (sourceProcessWithStreams)
import Data.IORef
  ( atomicModifyIORef,
    newIORef,
    readIORef,
  )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.UUID (UUID)
import Hercules.API.Agent.Evaluate
  ( getDerivationStatus2,
    tasksUpdateEvaluation,
  )
import qualified Hercules.API.Agent.Evaluate.DerivationStatus as DerivationStatus
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent as EvaluateEvent
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeErrorEvent as AttributeErrorEvent
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEvent as AttributeEvent
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.BuildRequest as BuildRequest
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.BuildRequired as BuildRequired
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo as DerivationInfo
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.JobConfig as JobConfig
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.Message as Message
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.PushedAll as PushedAll
import qualified Hercules.API.Agent.Evaluate.EvaluateTask as EvaluateTask
import Hercules.API.Servant (noContent)
import qualified Hercules.Agent.Cache as Agent.Cache
import qualified Hercules.Agent.Cachix.Env as Cachix.Env
import qualified Hercules.Agent.Client
import qualified Hercules.Agent.Config as Config
import Hercules.Agent.Env
import qualified Hercules.Agent.Env as Env
import Hercules.Agent.Evaluate.TraversalQueue (Queue)
import qualified Hercules.Agent.Evaluate.TraversalQueue as TraversalQueue
import Hercules.Agent.Files
import Hercules.Agent.Log
import qualified Hercules.Agent.Nix as Nix
import Hercules.Agent.Nix.RetrieveDerivationInfo
  ( retrieveDerivationInfo,
  )
import Hercules.Agent.NixFile (findNixFile)
import Hercules.Agent.NixFile.GitSource (fromRefRevPath)
import Hercules.Agent.NixPath
  ( renderSubPath,
  )
import Hercules.Agent.Producer
import Hercules.Agent.Sensitive (Sensitive (Sensitive))
import qualified Hercules.Agent.ServiceInfo as ServiceInfo
import Hercules.Agent.WorkerProcess ()
import qualified Hercules.Agent.WorkerProcess as WorkerProcess
import qualified Hercules.Agent.WorkerProtocol.Command as Command
import qualified Hercules.Agent.WorkerProtocol.Command.BuildResult as BuildResult
import qualified Hercules.Agent.WorkerProtocol.Command.Eval as Eval
import Hercules.Agent.WorkerProtocol.Event (ViaJSON (ViaJSON))
import qualified Hercules.Agent.WorkerProtocol.Event as Event
import qualified Hercules.Agent.WorkerProtocol.Event.Attribute as WorkerAttribute
import qualified Hercules.Agent.WorkerProtocol.Event.AttributeError as WorkerAttributeError
import qualified Hercules.Agent.WorkerProtocol.LogSettings as LogSettings
import Hercules.CNix.Store (Store, StorePath, parseStorePath)
import Hercules.Error (defaultRetry, quickRetry)
import qualified Network.HTTP.Client.Conduit as HTTP.Conduit
import qualified Network.HTTP.Simple as HTTP.Simple
import qualified Network.URI
import Protolude hiding (finally, newChan, writeChan)
import qualified Servant.Client
import Servant.Client.Core (showBaseUrl)
import qualified System.Directory as Dir
import System.FilePath
import System.Process

eventLimit :: Int
eventLimit = 50000

pushEvalWorkers :: Int
pushEvalWorkers = 16

performEvaluation :: Store -> EvaluateTask.EvaluateTask -> App ()
performEvaluation store task' =
  withProducer (produceEvaluationTaskEvents store task') $ \producer ->
    withBoundedDelayBatchProducer (1000 * 1000) 1000 producer $ \batchProducer ->
      fix $ \continue ->
        joinSTM $ listen batchProducer (\b -> withSync b (postBatch task' . catMaybes) *> continue) pure

produceEvaluationTaskEvents ::
  Store ->
  EvaluateTask.EvaluateTask ->
  (Syncing EvaluateEvent.EvaluateEvent -> App ()) ->
  App ()
produceEvaluationTaskEvents store task writeToBatch = withWorkDir "eval" $ \tmpdir -> do
  logLocM DebugS "Retrieving evaluation task"
  let emitSingle = writeToBatch . Syncable
      sync = syncer writeToBatch
  inputLocations <-
    EvaluateTask.otherInputs task
      & M.traverseWithKey \k src -> do
        let fetchName = tmpdir </> ("fetch-" <> toS k)
            argName = tmpdir </> ("arg-" <> toS k)
            metaName = do
              meta <- EvaluateTask.inputMetadata task & M.lookup k
              nameValue <- meta & M.lookup "name"
              name <- case A.fromJSON nameValue of
                A.Success a -> pure a
                _ -> Nothing
              guard (isValidName name)
              pure name
            destName
              | Just sourceName <- metaName = argName </> sourceName
              | otherwise = argName
        fetched <- fetchSource fetchName src
        liftIO do
          Dir.createDirectoryIfMissing True (takeDirectory destName)
          renamePathTryHarder fetched destName
        pure destName
  projectDir <- case M.lookup "src" inputLocations of
    Nothing -> panic "No primary source provided"
    Just x -> pure x
  (ref, rev) <- case M.lookup "src" (EvaluateTask.inputMetadata task) of
    Nothing -> do
      panic $ "No primary source metadata provided" <> show task
    Just meta -> pure $ fromMaybe (panic "no ref/rev in primary source metadata") do
      (,) <$> (meta ^? at "ref" . traverse . _String)
        <*> (meta ^? at "rev" . traverse . _String)
  nixPath <-
    EvaluateTask.nixPath task
      & ( traverse
            . traverse
            . traverse
            $ \identifier -> case M.lookup identifier inputLocations of
              Just x -> pure x
              Nothing ->
                throwIO $
                  FatalError $
                    "Nix path references undefined input "
                      <> identifier
        )
  autoArguments' <-
    EvaluateTask.autoArguments task
      & (traverse . traverse)
        ( \identifier -> case M.lookup identifier inputLocations of
            Just x | "/" `isPrefixOf` x -> pure x
            Just x ->
              throwIO $
                FatalError $
                  "input "
                    <> identifier
                    <> " was not resolved to an absolute path: "
                    <> toS x
            Nothing ->
              throwIO $
                FatalError $
                  "auto argument references undefined input "
                    <> identifier
        )
  let autoArguments =
        autoArguments'
          & M.mapWithKey \k sp ->
            let argPath = encodeUtf8 $ renderSubPath $ toS <$> sp
             in case do
                  inputId <- EvaluateTask.autoArguments task & M.lookup k
                  EvaluateTask.inputMetadata task & M.lookup (EvaluateTask.path inputId) of
                  Nothing -> Eval.ExprArg argPath
                  Just attrs ->
                    Eval.ExprArg $
                      -- TODO pass directly to avoid having to escape (or just escape properly)
                      "builtins.fromJSON ''" <> BL.toStrict (A.encode attrs) <> "'' // { outPath = " <> argPath <> "; }"
  msgCounter <- liftIO $ newIORef 0
  let fixIndex ::
        MonadIO m =>
        EvaluateEvent.EvaluateEvent ->
        m EvaluateEvent.EvaluateEvent
      fixIndex (EvaluateEvent.Message m) = do
        i <- liftIO $ atomicModifyIORef msgCounter (\i0 -> (i0 + 1, i0))
        pure $ EvaluateEvent.Message m {Message.index = i}
      fixIndex other = pure other
  eventCounter <- liftIO $ newIORef 0
  topDerivationPaths <- liftIO $ newIORef mempty
  let emit :: EvaluateEvent.EvaluateEvent -> App ()
      emit update = do
        n <- liftIO $ atomicModifyIORef eventCounter $ \n -> dup (n + 1)
        if n > eventLimit
          then do
            truncMsg <-
              fixIndex $
                EvaluateEvent.Message
                  Message.Message
                    { index = -1,
                      typ = Message.Error,
                      message =
                        "Evaluation limit reached. Does your nix expression produce infinite attributes? Please make sure that your project is finite. If it really does require more than "
                          <> show eventLimit
                          <> " attributes or messages, please contact info@hercules-ci.com."
                    }
            emitSingle truncMsg
            panic "Evaluation limit reached."
          else emitSingle =<< fixIndex update
  let allowedPaths = toList inputLocations <&> toS <&> encodeUtf8
  adHocSystem <-
    readFileMaybe (projectDir </> "ci-default-system.txt")
  liftIO (findNixFile projectDir) >>= \case
    Left e ->
      emit $
        EvaluateEvent.Message
          Message.Message
            { Message.index = -1, -- will be set by emit
              Message.typ = Message.Error,
              Message.message = e
            }
    Right file -> TraversalQueue.with $ \(derivationQueue :: Queue StorePath) ->
      let doIt = do
            Async.Lifted.concurrently_ evaluation emitDrvs
            -- derivationInfo upload has finished
            -- allAttrPaths :: IORef has been populated
            pushDrvs
          uploadDrvInfos drvPath = do
            TraversalQueue.enqueue derivationQueue drvPath
            TraversalQueue.waitUntilDone derivationQueue
          addTopDerivation drvPath = do
            TraversalQueue.enqueue derivationQueue drvPath
            liftIO $ atomicModifyIORef topDerivationPaths ((,()) . S.insert drvPath)
          evaluation = do
            Nix.withExtraOptions [("system", T.strip s) | Just s <- [adHocSystem]] $
              runEvalProcess
                projectDir
                file
                autoArguments
                nixPath
                captureAttrDrvAndEmit
                uploadDrvInfos
                sync
                (EvaluateTask.logToken task)
                (ref, rev)
                (EvaluateTask.selector task)
                allowedPaths
            -- process has finished
            TraversalQueue.waitUntilDone derivationQueue
            TraversalQueue.close derivationQueue
          pushDrvs = do
            caches <- activePushCaches
            paths <- liftIO $ readIORef topDerivationPaths
            forM_ caches $ \cache -> do
              withNamedContext "cache" cache $ logLocM DebugS "Pushing derivations"
              Agent.Cache.push cache (toList paths) pushEvalWorkers
              emit $ EvaluateEvent.PushedAll $ PushedAll.PushedAll {cache = cache}
          captureAttrDrvAndEmit msg = do
            case msg of
              EvaluateEvent.Attribute ae -> do
                storePath <- liftIO $ parseStorePath store (encodeUtf8 $ AttributeEvent.derivationPath ae)
                addTopDerivation storePath
              _ -> pass
            emit msg
          emitDrvs =
            TraversalQueue.work derivationQueue $ \recurse drvPath -> do
              drvInfo <- retrieveDerivationInfo store drvPath
              for_ (M.keys $ DerivationInfo.inputDerivations drvInfo) \inp -> do
                inputStorePath <- liftIO $ parseStorePath store (encodeUtf8 inp)
                recurse inputStorePath -- asynchronously
              emit $ EvaluateEvent.DerivationInfo drvInfo
       in doIt

isValidName :: FilePath -> Bool
isValidName "" = False
isValidName cs@(c0 : _) = all isValidNameChar cs && c0 /= '.'
  where
    isValidNameChar c =
      isAsciiUpper c
        || isAsciiLower c
        || isDigit c
        || c `elem` ("+-._?=" :: [Char])

runEvalProcess ::
  FilePath ->
  FilePath ->
  Map Text Eval.Arg ->
  [ EvaluateTask.NixPathElement
      (EvaluateTask.SubPathOf FilePath)
  ] ->
  (EvaluateEvent.EvaluateEvent -> App ()) ->
  -- | Upload a derivation, return when done
  (StorePath -> App ()) ->
  App () ->
  Text ->
  (Text, Text) ->
  EvaluateTask.Selector ->
  [ByteString] ->
  App ()
runEvalProcess projectDir file autoArguments nixPath emit uploadDerivationInfos flush logToken (ref, rev) selector allowedPaths = do
  extraOpts <- Nix.askExtraOptions
  bulkBaseURL <- asks (ServiceInfo.bulkSocketBaseURL . Env.serviceInfo)
  apiBaseUrl <- asks (toS . showBaseUrl . Env.herculesBaseUrl)
  cfg <- asks Env.config
  let gitSource = fromRefRevPath ref rev (toS projectDir)
      eval =
        Eval.Eval
          { Eval.cwd = projectDir,
            Eval.file = toS file,
            Eval.autoArguments = autoArguments,
            Eval.extraNixOptions = extraOpts,
            Eval.logSettings =
              LogSettings.LogSettings
                { token = Sensitive logToken,
                  path = "/api/v1/logs/build/socket",
                  baseURL = toS $ Network.URI.uriToString identity bulkBaseURL ""
                },
            Eval.gitSource = ViaJSON gitSource,
            Eval.apiBaseUrl = apiBaseUrl,
            Eval.selector = ViaJSON selector,
            Eval.allowInsecureBuiltinFetchers = Config.allowInsecureBuiltinFetchers cfg,
            Eval.allowedPaths = allowedPaths
          }
  buildRequiredIndex <- liftIO $ newIORef (0 :: Int)
  commandChan <- newChan
  writeChan commandChan $ Just $ Command.Eval eval
  let decode = decodeUtf8With lenientDecode
  withProducer (produceWorkerEvents eval nixPath commandChan) $
    \workerEventsP -> fix $ \continue ->
      joinSTM $
        listen
          workerEventsP
          ( \case
              Event.Attribute a -> do
                emit $
                  EvaluateEvent.Attribute $
                    AttributeEvent.AttributeEvent
                      { AttributeEvent.expressionPath = decode <$> WorkerAttribute.path a,
                        AttributeEvent.derivationPath = decode $ WorkerAttribute.drv a,
                        AttributeEvent.typ = case WorkerAttribute.typ a of
                          WorkerAttribute.Regular -> AttributeEvent.Regular
                          WorkerAttribute.MustFail -> AttributeEvent.MustFail
                          WorkerAttribute.MayFail -> AttributeEvent.MayFail
                          WorkerAttribute.Effect -> AttributeEvent.Effect
                          WorkerAttribute.DependenciesOnly -> AttributeEvent.DependenciesOnly
                      }
                continue
              Event.AttributeError e -> do
                emit $
                  EvaluateEvent.AttributeError $
                    AttributeErrorEvent.AttributeErrorEvent
                      { AttributeErrorEvent.expressionPath = decode <$> WorkerAttributeError.path e,
                        AttributeErrorEvent.errorMessage = WorkerAttributeError.message e,
                        AttributeErrorEvent.errorType = WorkerAttributeError.errorType e,
                        AttributeErrorEvent.errorDerivation = WorkerAttributeError.errorDerivation e
                      }
                continue
              Event.EvaluationDone ->
                writeChan commandChan Nothing
              Event.Error e -> do
                emit $
                  EvaluateEvent.Message
                    Message.Message
                      { Message.index = -1, -- will be set by emit
                        Message.typ = Message.Error,
                        Message.message = e
                      }
                continue
              Event.Build drv outputName notAttempt -> do
                store <- asks (Cachix.Env.nixStore . Env.cachixEnv)
                storePath <- liftIO (parseStorePath store drv)
                let drvText = decode drv
                status <-
                  withNamedContext "derivation" (decode drv) $ do
                    currentIndex <- liftIO $ atomicModifyIORef buildRequiredIndex (\i -> (i + 1, i))
                    emit $
                      EvaluateEvent.BuildRequired
                        BuildRequired.BuildRequired
                          { BuildRequired.derivationPath = drvText,
                            BuildRequired.index = currentIndex,
                            BuildRequired.outputName = outputName
                          }
                    let pushDerivations = do
                          caches <- activePushCaches
                          forM_ caches $ \cache -> do
                            withNamedContext "cache" cache $ logLocM DebugS "Pushing derivations for import from derivation"
                            Agent.Cache.push cache [storePath] pushEvalWorkers
                    Async.Lifted.concurrently_
                      (uploadDerivationInfos storePath)
                      pushDerivations
                    emit $
                      EvaluateEvent.BuildRequest
                        BuildRequest.BuildRequest
                          { derivationPath = drvText,
                            forceRebuild = isJust notAttempt
                          }
                    flush
                    status <- drvPoller notAttempt drvText
                    logLocM DebugS $ "Got derivation status " <> logStr (show status :: Text)
                    return status
                writeChan commandChan $ Just $ Command.BuildResult $ uncurry (BuildResult.BuildResult drvText) status
                continue
              Event.OnPushHandler (ViaJSON e) -> do
                emit $ EvaluateEvent.OnPushHandlerEvent e
                continue
              Event.JobConfig -> do
                emit $ EvaluateEvent.JobConfig JobConfig.JobConfig {sourceCaches = Nothing, binaryCaches = Nothing}
                continue
              Event.Exception e -> panic e
              -- Unused during eval
              Event.BuildResult {} -> pass
              Event.EffectResult {} -> pass
          )
          ( \case
              ExitSuccess -> logLocM DebugS "Clean worker exit"
              ExitFailure e -> do
                withNamedContext "exitStatus" e $ logLocM ErrorS "Worker failed"
                panic $ "Worker failed with exit status: " <> show e
          )

produceWorkerEvents ::
  Eval.Eval ->
  [EvaluateTask.NixPathElement (EvaluateTask.SubPathOf FilePath)] ->
  Chan (Maybe Command.Command) ->
  (Event.Event -> App ()) ->
  App ExitCode
produceWorkerEvents eval nixPath commandChan writeEvent = do
  workerExe <- WorkerProcess.getWorkerExe
  let opts = [show $ Eval.extraNixOptions eval]
  -- NiceToHave: replace renderNixPath by something structured like -I
  -- to support = and : in paths
  workerEnv <- liftIO $ WorkerProcess.prepareEnv (WorkerProcess.WorkerEnvSettings {nixPath = nixPath})
  let wps =
        (System.Process.proc workerExe opts)
          { env = Just workerEnv,
            close_fds = True, -- Disable on Windows?
            cwd = Just (Eval.cwd eval)
          }
  WorkerProcess.runWorker wps (stderrLineHandler "evaluator") commandChan writeEvent

drvPoller :: Maybe UUID -> Text -> App (UUID, BuildResult.BuildStatus)
drvPoller notAttempt drvPath = do
  resp <-
    defaultRetry $
      runHerculesClient $
        getDerivationStatus2
          Hercules.Agent.Client.evalClient
          drvPath
  let oneSecond = 1000 * 1000
      again = do
        liftIO $ threadDelay oneSecond
        drvPoller notAttempt drvPath
  case resp of
    Nothing -> again
    Just (attempt, _) | Just attempt == notAttempt -> again
    Just (_attempt, DerivationStatus.Waiting) -> again
    Just (_attempt, DerivationStatus.Building) -> again
    Just (attempt, DerivationStatus.BuildFailure) -> pure (attempt, BuildResult.Failure)
    Just (attempt, DerivationStatus.DependencyFailure) -> pure (attempt, BuildResult.DependencyFailure)
    Just (attempt, DerivationStatus.BuildSuccess) -> pure (attempt, BuildResult.Success)

newtype SubprocessFailure = SubprocessFailure {message :: Text}
  deriving (Typeable, Exception, Show)

fetchSource :: FilePath -> Text -> App FilePath
fetchSource targetDir url = do
  clientEnv <- asks herculesClientEnv
  liftIO $ Dir.createDirectoryIfMissing True targetDir
  request <- HTTP.Simple.parseRequest $ toS url
  -- TODO: report stderr to service
  -- TODO: discard stdout
  -- Fewer retries in order to speed up the tests.
  quickRetry $ do
    (x, _, _) <-
      liftIO $
        (`runReaderT` Servant.Client.manager clientEnv) $
          HTTP.Conduit.withResponse request $
            \response -> do
              let tarball = HTTP.Conduit.responseBody response
                  procSpec =
                    (System.Process.proc "tar" ["-xz"]) {cwd = Just targetDir}
              sourceProcessWithStreams
                procSpec
                tarball
                Conduit.stderrC
                Conduit.stderrC
    case x of
      ExitSuccess -> pass
      ExitFailure {} -> throwIO $ SubprocessFailure "Extracting tarball"
    liftIO $ findTarballDir targetDir

dup :: a -> (a, a)
dup a = (a, a)

-- | Tarballs typically have a single directory at the root to cd into.
findTarballDir :: FilePath -> IO FilePath
findTarballDir fp = do
  nodes <- Dir.listDirectory fp
  case nodes of
    [x] ->
      Dir.doesDirectoryExist (fp </> x) >>= \case
        True -> pure $ fp </> x
        False -> pure fp
    _ -> pure fp

postBatch :: EvaluateTask.EvaluateTask -> [EvaluateEvent.EvaluateEvent] -> App ()
postBatch task events =
  noContent $
    defaultRetry $
      runHerculesClient
        ( tasksUpdateEvaluation
            Hercules.Agent.Client.evalClient
            (EvaluateTask.id task)
            events
        )
