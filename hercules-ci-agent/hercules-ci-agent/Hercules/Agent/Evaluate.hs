{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Agent.Evaluate
  ( performEvaluation
    )
where

import Conduit
import qualified Control.Concurrent.Async.Lifted as Async.Lifted
import Control.Concurrent.Chan.Lifted
import Control.Monad.IO.Unlift
import Data.Conduit.Process (sourceProcessWithStreams)
import Data.IORef
  ( atomicModifyIORef,
    newIORef,
    readIORef
    )
import qualified Data.Map as M
import qualified Data.Set as S
import Hercules.API (noContent)
import Hercules.API.Agent.Evaluate
  ( getDerivationStatus,
    tasksGetEvaluation,
    tasksUpdateEvaluation
    )
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent as EvaluateEvent
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeErrorEvent as AttributeErrorEvent
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEvent as AttributeEvent
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.BuildRequest as BuildRequest
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.BuildRequired as BuildRequired
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo as DerivationInfo
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.PushedAll as PushedAll
import qualified Hercules.API.Agent.Evaluate.EvaluateTask as EvaluateTask
import qualified Hercules.API.Derivation as Derivation
import qualified Hercules.API.Message as Message
import Hercules.API.Task (Task)
import qualified Hercules.API.Task as Task
import qualified Hercules.Agent.Cachix as Agent.Cachix
import qualified Hercules.Agent.Client
import qualified Hercules.Agent.Config as Config
import Hercules.Agent.Env
import qualified Hercules.Agent.Evaluate.TraversalQueue as TraversalQueue
import Hercules.Agent.Exception (defaultRetry, quickRetry)
import Hercules.Agent.Log
import qualified Hercules.Agent.Nix as Nix
import Hercules.Agent.Nix.RetrieveDerivationInfo
  ( retrieveDerivationInfo
    )
import Hercules.Agent.NixPath
  ( renderNixPath,
    renderSubPath
    )
import Hercules.Agent.Producer
import Hercules.Agent.WorkerProcess
import qualified Hercules.Agent.WorkerProtocol.Command as Command
import qualified Hercules.Agent.WorkerProtocol.Command.BuildResult as BuildResult
import qualified Hercules.Agent.WorkerProtocol.Command.Eval as Eval
import qualified Hercules.Agent.WorkerProtocol.Event as Event
import qualified Hercules.Agent.WorkerProtocol.Event.Attribute as WorkerAttribute
import qualified Hercules.Agent.WorkerProtocol.Event.AttributeError as WorkerAttributeError
import qualified Network.HTTP.Client.Conduit as HTTP.Conduit
import qualified Network.HTTP.Simple as HTTP.Simple
import Paths_hercules_ci_agent (getBinDir)
import Protolude hiding (finally, newChan, writeChan)
import qualified Servant.Client
import qualified System.Directory as Dir
import System.FilePath
import System.IO.Temp (withTempDirectory)
import System.Process

eventLimit :: Int
eventLimit = 50000

pushEvalWorkers :: Int
pushEvalWorkers = 16

performEvaluation :: Task EvaluateTask.EvaluateTask -> App ()
performEvaluation task' =
  withProducer (produceEvaluationTaskEvents task') $ \producer ->
    withBoundedDelayBatchProducer (1000 * 1000) 1000 producer $ \batchProducer ->
      fix $ \continue ->
        joinSTM $ listen batchProducer (\b -> withSync b (postBatch task' . catMaybes) *> continue) pure

produceEvaluationTaskEvents
  :: Task EvaluateTask.EvaluateTask
  -> (Syncing EvaluateEvent.EvaluateEvent -> App ())
  -> App ()
produceEvaluationTaskEvents task' writeToBatch = withWorkDir $ \tmpdir -> do
  logLocM DebugS "Retrieving evaluation task"
  task <-
    defaultRetry
      $ runHerculesClient
          (tasksGetEvaluation Hercules.Agent.Client.evalClient (Task.id task'))
  let emitSingle = writeToBatch . Syncable
      sync = syncer writeToBatch
  projectDir <-
    fetchSource (tmpdir </> "primary")
      (EvaluateTask.primaryInput task)
  withNamedContext "projectDir" projectDir
    $ logLocM DebugS "Determined projectDir"
  inputLocations <-
    flip M.traverseWithKey (EvaluateTask.otherInputs task)
      $ \k src -> fetchSource (tmpdir </> ("arg-" <> toS k)) src
  nixPath <-
    EvaluateTask.nixPath task
      & ( traverse
            . traverse
            . traverse
            $ \identifier -> case M.lookup identifier inputLocations of
              Just x -> pure x
              Nothing ->
                throwIO
                  $ FatalError
                  $ "Nix path references undefined input "
                  <> identifier
          )
  autoArguments' <-
    EvaluateTask.autoArguments task
      & (traverse . traverse)
          ( \identifier -> case M.lookup identifier inputLocations of
              Just x | "/" `isPrefixOf` x -> pure x
              Just x ->
                throwIO
                  $ FatalError
                  $ "input "
                  <> identifier
                  <> " was not resolved to an absolute path: "
                  <> toS x
              Nothing ->
                throwIO
                  $ FatalError
                  $ "auto argument references undefined input "
                  <> identifier
            )
  let autoArguments = autoArguments'
        <&> \sp -> Eval.ExprArg $ toS $ renderSubPath $ toS <$> sp
  msgCounter <- liftIO $ newIORef 0
  let fixIndex
        :: MonadIO m
        => EvaluateEvent.EvaluateEvent
        -> m EvaluateEvent.EvaluateEvent
      fixIndex (EvaluateEvent.Message m) = do
        i <- liftIO $ atomicModifyIORef msgCounter (\i0 -> (i0 + 1, i0))
        pure $ EvaluateEvent.Message m {Message.index = i}
      fixIndex other = pure other
  eventCounter <- liftIO $ newIORef 0
  allAttrPaths <- liftIO $ newIORef mempty
  let emit :: EvaluateEvent.EvaluateEvent -> App ()
      emit update = do
        n <- liftIO $ atomicModifyIORef eventCounter $ \n -> dup (n + 1)
        if n > eventLimit
          then
            do
              truncMsg <-
                fixIndex
                  $ EvaluateEvent.Message Message.Message
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
  liftIO (findNixFile projectDir) >>= \case
    Left e ->
      emit
        $ EvaluateEvent.Message Message.Message
            { Message.index = -1, -- will be set by emit
              Message.typ = Message.Error,
              Message.message = e
              }
    Right file -> TraversalQueue.with $ \derivationQueue ->
      let doIt = do
            Async.Lifted.concurrently_ evaluation emitDrvs
            -- derivationInfo upload has finished
            -- allAttrPaths :: IORef has been populated
            pushDrvs
          evaluation = do
            runEvalProcess projectDir
              file
              autoArguments
              nixPath
              captureAttrDrvAndEmit
              derivationQueue
              sync
            -- process has finished
            TraversalQueue.waitUntilDone derivationQueue
            TraversalQueue.close derivationQueue
          pushDrvs = do
            caches <- Agent.Cachix.activePushCaches
            paths <- liftIO $ readIORef allAttrPaths
            forM_ caches $ \cache -> do
              withNamedContext "cache" cache $ logLocM DebugS "Pushing drvs to cachix"
              Agent.Cachix.push cache (toList paths) pushEvalWorkers
              emit $ EvaluateEvent.PushedAll $ PushedAll.PushedAll {cache = cache}
          captureAttrDrvAndEmit msg = do
            case msg of
              EvaluateEvent.Attribute ae ->
                TraversalQueue.enqueue
                  derivationQueue
                  (AttributeEvent.derivationPath ae)
              _ -> pass
            emit msg
          emitDrvs =
            TraversalQueue.work derivationQueue $ \recurse drvPath -> do
              liftIO $ atomicModifyIORef allAttrPaths ((,()) . S.insert drvPath)
              drvInfo <- retrieveDerivationInfo drvPath
              forM_ (M.keys $ DerivationInfo.inputDerivations drvInfo)
                recurse -- asynchronously
              emit $ EvaluateEvent.DerivationInfo drvInfo
       in doIt

runEvalProcess
  :: FilePath
  -> FilePath
  -> Map Text Eval.Arg
  -> [ EvaluateTask.NixPathElement
         (EvaluateTask.SubPathOf FilePath)
       ]
  -> (EvaluateEvent.EvaluateEvent -> App ())
  -> TraversalQueue.Queue Text
  -> App ()
  -> App ()
runEvalProcess projectDir file autoArguments nixPath emit derivationQueue flush = do
  extraOpts <- Nix.askExtraOptions
  let eval = Eval.Eval
        { Eval.cwd = projectDir,
          Eval.file = toS file,
          Eval.autoArguments = autoArguments,
          Eval.extraNixOptions = extraOpts
          }
  buildRequiredIndex <- liftIO $ newIORef (0 :: Int)
  commandChan <- newChan
  writeChan commandChan $ Just $ Command.Eval eval
  withProducer (produceWorkerEvents eval nixPath commandChan)
    $ \workerEventsP -> fix $ \continue ->
      joinSTM
        $ listen workerEventsP
            ( \case
                Event.Attribute a -> do
                  emit $ EvaluateEvent.Attribute $ AttributeEvent.AttributeEvent
                    { AttributeEvent.expressionPath = toSL <$> WorkerAttribute.path a,
                      AttributeEvent.derivationPath = toSL $ WorkerAttribute.drv a
                      }
                  continue
                Event.AttributeError e -> do
                  emit $ EvaluateEvent.AttributeError $ AttributeErrorEvent.AttributeErrorEvent
                    { AttributeErrorEvent.expressionPath = toSL <$> WorkerAttributeError.path e,
                      AttributeErrorEvent.errorMessage = toSL $ WorkerAttributeError.message e,
                      AttributeErrorEvent.errorType = toSL <$> WorkerAttributeError.errorType e,
                      AttributeErrorEvent.errorDerivation = toSL <$> WorkerAttributeError.errorDerivation e
                      }
                  continue
                Event.EvaluationDone ->
                  writeChan commandChan Nothing
                Event.Error e -> do
                  emit
                    $ EvaluateEvent.Message Message.Message
                        { Message.index = -1, -- will be set by emit
                          Message.typ = Message.Error,
                          Message.message = e
                          }
                  continue
                Event.Build drv outputName -> do
                  status <-
                    withNamedContext "derivation" drv $ do
                      currentIndex <- liftIO $ atomicModifyIORef buildRequiredIndex (\i -> (i + 1, i))
                      emit
                        $ EvaluateEvent.BuildRequired BuildRequired.BuildRequired
                            { BuildRequired.derivationPath = drv,
                              BuildRequired.index = currentIndex,
                              BuildRequired.outputName = outputName
                              }
                      let submitDerivationInfos = do
                            TraversalQueue.enqueue derivationQueue drv
                            TraversalQueue.waitUntilDone derivationQueue
                          pushDerivations = do
                            caches <- Agent.Cachix.activePushCaches
                            forM_ caches $ \cache -> do
                              withNamedContext "cache" cache $ logLocM DebugS "Pushing ifd drvs to cachix"
                              Agent.Cachix.push cache [drv] pushEvalWorkers
                      Async.Lifted.concurrently_ submitDerivationInfos pushDerivations
                      emit $ EvaluateEvent.BuildRequest BuildRequest.BuildRequest {BuildRequest.derivationPath = drv}
                      flush
                      status <- drvPoller drv
                      logLocM DebugS $ "Got derivation status " <> show status
                      return status
                  writeChan commandChan $ Just $ Command.BuildResult $ BuildResult.BuildResult drv status
                  continue
              )
            ( \case
                ExitSuccess -> logLocM DebugS "Clean worker exit"
                ExitFailure e -> do
                  withNamedContext "exitStatus" e $ logLocM ErrorS "Worker failed"
                  panic $ "Worker failed with exit status: " <> show e
              )

produceWorkerEvents
  :: Eval.Eval
  -> [EvaluateTask.NixPathElement (EvaluateTask.SubPathOf FilePath)]
  -> Chan (Maybe Command.Command)
  -> (Event.Event -> App ())
  -> App ExitCode
produceWorkerEvents eval nixPath commandChan writeEvent = do
  workerBinDir <- liftIO getBinDir
  let opts = [show $ Eval.extraNixOptions eval]
  -- NiceToHave: replace renderNixPath by something structured like -I
  -- to support = and : in paths
  wps <-
    pure
      (System.Process.proc (workerBinDir </> "hercules-ci-agent-worker") opts)
        { env = Just [("NIX_PATH", toS $ renderNixPath nixPath)],
          close_fds = True, -- Disable on Windows?
          cwd = Just (Eval.cwd eval)
          }
  runWorker wps stderrLineHandler commandChan writeEvent

drvPoller :: Text -> App BuildResult.BuildStatus
drvPoller drvPath = do
  resp <-
    defaultRetry $ runHerculesClient
      $ getDerivationStatus
          Hercules.Agent.Client.evalClient
          drvPath
  let oneSecond = 1000 * 1000
      again = do
        liftIO $ threadDelay oneSecond
        drvPoller drvPath
  case resp of
    Nothing -> again
    Just Derivation.Waiting -> again
    Just Derivation.Building -> again
    Just Derivation.BuildFailure -> pure BuildResult.Failure
    Just Derivation.DependencyFailure -> pure BuildResult.DependencyFailure
    Just Derivation.BuildSuccess -> pure BuildResult.Success

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
      liftIO
        $ (`runReaderT` Servant.Client.manager clientEnv)
        $ HTTP.Conduit.withResponse request
        $ \response -> do
          let tarball = HTTP.Conduit.responseBody response
              procSpec =
                (System.Process.proc "tar" ["-xz"]) {cwd = Just targetDir}
          sourceProcessWithStreams procSpec
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
    [x] -> Dir.doesDirectoryExist (fp </> x) >>= \case
      True -> pure $ fp </> x
      False -> pure fp
    _ -> pure fp

type Ambiguity = [FilePath]

searchPath :: [Ambiguity]
searchPath = [["nix/ci.nix", "ci.nix"], ["default.nix"]]

findNixFile :: FilePath -> IO (Either Text FilePath)
findNixFile projectDir = do
  searchResult <-
    for searchPath $ traverse $ \relPath ->
      let path = projectDir </> relPath
       in Dir.doesFileExist path >>= \case
            True -> pure $ Just (relPath, path)
            False -> pure Nothing
  case filter (not . null) $ map catMaybes searchResult of
    [(_relPath, unambiguous)] : _ -> pure (pure unambiguous)
    ambiguous : _ ->
      pure
        $ Left
        $ "Don't know what to do, expecting only one of "
        <> englishConjunction "or" (map fst ambiguous)
    [] ->
      pure
        $ Left
        $ "Please provide a Nix expression to build. Could not find any of "
        <> englishConjunction "or" (concat searchPath)
        <> " in your source"

englishConjunction :: Show a => Text -> [a] -> Text
englishConjunction _ [] = "none"
englishConjunction _ [a] = show a
englishConjunction connective [a1, a2] =
  show a1 <> " " <> connective <> " " <> show a2
englishConjunction connective (a : as) =
  show a <> ", " <> englishConjunction connective as

stderrLineHandler :: Int -> ByteString -> App ()
stderrLineHandler pid ln =
  withNamedContext "worker" (pid :: Int)
    $ logLocM InfoS
    $ "Evaluator: "
    <> logStr (toSL ln :: Text)

postBatch :: Task EvaluateTask.EvaluateTask -> [EvaluateEvent.EvaluateEvent] -> App ()
postBatch task events =
  noContent $ defaultRetry
    $ runHerculesClient
        ( tasksUpdateEvaluation Hercules.Agent.Client.evalClient
            (Task.id task)
            events
          )

-- TODO: configurable temp directory
withWorkDir :: (FilePath -> App b) -> App b
withWorkDir f = do
  UnliftIO {unliftIO = unlift} <- askUnliftIO
  workDir <- asks (Config.workDirectory . config)
  liftIO $ withTempDirectory workDir "eval" $ unlift . f
