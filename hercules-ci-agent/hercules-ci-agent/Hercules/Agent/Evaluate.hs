{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Agent.Evaluate
  ( performEvaluation,
  )
where

import Conduit
import Control.Concurrent.Async.Lifted qualified as Async.Lifted
import Control.Concurrent.Chan.Lifted
import Control.Exception.Lifted (finally)
import Control.Exception.Safe qualified as Safe
import Control.Lens (at, (^?))
import Control.Monad.IO.Unlift (askUnliftIO, unliftIO)
import Data.Aeson qualified as A
import Data.Aeson.Lens (_String)
import Data.ByteString.Lazy qualified as BL
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Conduit.Process (sourceProcessWithStreams)
import Data.IORef
  ( atomicModifyIORef,
    newIORef,
    readIORef,
  )
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.UUID (UUID)
import Hercules.API (Id)
import Hercules.API.Agent.Evaluate
  ( getDerivationStatus2,
    tasksUpdateEvaluation,
  )
import Hercules.API.Agent.Evaluate.DerivationStatus qualified as DerivationStatus
import Hercules.API.Agent.Evaluate.EvaluateEvent qualified as EvaluateEvent
import Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEffectEvent qualified as AttributeEffectEvent
import Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeErrorEvent qualified as AttributeErrorEvent
import Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEvent qualified as AttributeEvent
import Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeIFDEvent qualified as AttributeIFDEvent
import Hercules.API.Agent.Evaluate.EvaluateEvent.BuildRequest qualified as BuildRequest
import Hercules.API.Agent.Evaluate.EvaluateEvent.BuildRequired qualified as BuildRequired
import Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo qualified as DerivationInfo
import Hercules.API.Agent.Evaluate.EvaluateEvent.JobConfig qualified as JobConfig
import Hercules.API.Agent.Evaluate.EvaluateEvent.Message qualified as Message
import Hercules.API.Agent.Evaluate.EvaluateEvent.PushedAll qualified as PushedAll
import Hercules.API.Agent.Evaluate.EvaluateTask qualified as EvaluateTask
import Hercules.API.Agent.Evaluate.ImmutableGitInput (ImmutableGitInput)
import Hercules.API.Agent.Evaluate.ImmutableGitInput qualified as ImmutableGitInput
import Hercules.API.Agent.Evaluate.ImmutableInput qualified as ImmutableInput
import Hercules.API.Servant (noContent)
import Hercules.API.Task (Task)
import Hercules.Agent.Cache qualified as Agent.Cache
import Hercules.Agent.Client qualified
import Hercules.Agent.Config qualified as Config
import Hercules.Agent.Env
import Hercules.Agent.Env qualified as Env
import Hercules.Agent.Evaluate.TraversalQueue (Queue)
import Hercules.Agent.Evaluate.TraversalQueue qualified as TraversalQueue
import Hercules.Agent.Files
import Hercules.Agent.Log
import Hercules.Agent.Netrc qualified as Netrc
import Hercules.Agent.Nix qualified as Nix
import Hercules.Agent.Nix.RetrieveDerivationInfo
  ( retrieveDerivationInfo,
  )
import Hercules.Agent.NixFile (findNixFile)
import Hercules.Agent.NixFile.GitSource qualified as GitSource
import Hercules.Agent.NixPath
  ( renderSubPath,
  )
import Hercules.Agent.Producer
import Hercules.Agent.Sensitive (Sensitive (Sensitive))
import Hercules.Agent.ServiceInfo qualified as ServiceInfo
import Hercules.Agent.WorkerProcess ()
import Hercules.Agent.WorkerProcess qualified as WorkerProcess
import Hercules.Agent.WorkerProtocol.Command qualified as Command
import Hercules.Agent.WorkerProtocol.Command.BuildResult qualified as BuildResult
import Hercules.Agent.WorkerProtocol.Command.Eval qualified as Eval
import Hercules.Agent.WorkerProtocol.Event qualified as Event
import Hercules.Agent.WorkerProtocol.Event.Attribute qualified as WorkerAttribute
import Hercules.Agent.WorkerProtocol.Event.AttributeError qualified as WorkerAttributeError
import Hercules.Agent.WorkerProtocol.Event.AttributeIFD qualified as AttributeIFD
import Hercules.Agent.WorkerProtocol.LogSettings qualified as LogSettings
import Hercules.Agent.WorkerProtocol.ViaJSON (ViaJSON (ViaJSON), fromViaJSON)
import Hercules.CNix.Store (Store, StorePath, parseStorePath)
import Hercules.CNix.Store qualified as CNix
import Hercules.Effect (parseDrvSecretsMap)
import Hercules.Error (defaultRetry, quickRetry)
import Network.HTTP.Client.Conduit qualified as HTTP.Conduit
import Network.HTTP.Simple qualified as HTTP.Simple
import Network.URI qualified
import Protolude hiding (finally, newChan, writeChan)
import Servant.Client qualified
import Servant.Client.Core (showBaseUrl)
import System.Directory qualified as Dir
import System.FilePath
import System.Process
import UnliftIO (atomicModifyIORef')

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

getSrcInput :: MonadIO m => EvaluateTask.EvaluateTask -> m (Maybe ImmutableGitInput)
getSrcInput task = case M.lookup "src" (EvaluateTask.inputs task) of
  Just (ImmutableInput.Git x) ->
    purer x
  Nothing -> do
    throwIO $ FatalError $ "No src input provided" <> show task
  Just {} -> do
    pure Nothing

makeEventEmitter ::
  (Syncing EvaluateEvent.EvaluateEvent -> App ()) ->
  App (EvaluateEvent.EvaluateEvent -> App ())
makeEventEmitter writeToBatch = do
  let emitSingle = writeToBatch . Syncable

  eventCounter <- liftIO $ newIORef 0
  msgCounter <- liftIO $ newIORef 0

  let fixIndex ::
        MonadIO m =>
        EvaluateEvent.EvaluateEvent ->
        m EvaluateEvent.EvaluateEvent
      fixIndex (EvaluateEvent.Message m) = do
        i <- liftIO $ atomicModifyIORef msgCounter (\i0 -> (i0 + 1, i0))
        pure $ EvaluateEvent.Message m {Message.index = i}
      fixIndex other = pure other

      isLimited :: EvaluateEvent.EvaluateEvent -> Bool
      isLimited = \case
        EvaluateEvent.Attribute {} -> True
        EvaluateEvent.AttributeEffect {} -> True
        EvaluateEvent.AttributeError {} -> True
        EvaluateEvent.Message {} ->
          -- a simplistic solution against unexpected runaway Messages.
          -- we don't expect >1 message, especially since we have a separate eval log now,
          -- so realistically it doesn't impact the attribute limit at all.
          True
        _ -> False

      emit update = do
        when (isLimited update) do
          n <- liftIO $ atomicModifyIORef eventCounter $ \n -> dup (n + 1)
          when (n > eventLimit) do
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
        emitSingle =<< fixIndex update
  pure emit

produceEvaluationTaskEvents ::
  Store ->
  EvaluateTask.EvaluateTask ->
  (Syncing EvaluateEvent.EvaluateEvent -> App ()) ->
  App ()
produceEvaluationTaskEvents store task writeToBatch | EvaluateTask.isFlakeJob task = withWorkDir "eval" $ \tmpdir -> do
  logLocM DebugS "Retrieving evaluation task (flake)"
  let sync = syncer writeToBatch
  topDerivationPaths <- liftIO $ newIORef mempty
  emit <- makeEventEmitter writeToBatch
  let allowedPaths = []

  TraversalQueue.with $ \(derivationQueue :: Queue StorePath) ->
    let doIt =
          ( do
              Async.Lifted.concurrently_ evaluation emitDrvs
              -- derivationInfo upload has finished
              -- allAttrPaths :: IORef has been populated
          )
            `finally` pushDrvs
        uploadDrvInfos drvPath = do
          TraversalQueue.enqueue derivationQueue drvPath
          TraversalQueue.waitUntilDone derivationQueue
        addTopDerivation drvPath = do
          TraversalQueue.enqueue derivationQueue drvPath
          liftIO $ atomicModifyIORef topDerivationPaths ((,()) . S.insert drvPath)
        evaluation = do
          let evalProc =
                do runEvalProcess
                  store
                  tmpdir -- unused
                  ""
                  mempty
                  mempty
                  captureAttrDrvAndEmit
                  uploadDrvInfos
                  sync
                  task
                  allowedPaths
          evalProc `finally` do
            -- Always upload drv infos, even in case of a crash in the worker
            TraversalQueue.waitUntilDone derivationQueue
              `finally` TraversalQueue.close derivationQueue
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
            EvaluateEvent.AttributeEffect ae -> do
              storePath <- liftIO $ parseStorePath store (encodeUtf8 $ AttributeEffectEvent.derivationPath ae)
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
produceEvaluationTaskEvents store task writeToBatch = withWorkDir "eval" $ \tmpdir -> do
  logLocM DebugS "Retrieving evaluation task"
  let sync = syncer writeToBatch
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

  topDerivationPaths <- liftIO $ newIORef mempty

  emit <- makeEventEmitter writeToBatch

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
      let doIt =
            ( do
                Async.Lifted.concurrently_ evaluation emitDrvs
                -- derivationInfo upload has finished
                -- allAttrPaths :: IORef has been populated
            )
              `finally` pushDrvs
          uploadDrvInfos drvPath = do
            TraversalQueue.enqueue derivationQueue drvPath
            TraversalQueue.waitUntilDone derivationQueue
          addTopDerivation drvPath = do
            TraversalQueue.enqueue derivationQueue drvPath
            liftIO $ atomicModifyIORef topDerivationPaths ((,()) . S.insert drvPath)
          evaluation = do
            Nix.withExtraOptions [("system", T.strip s) | Just s <- [adHocSystem]] do
              let evalProc =
                    do runEvalProcess
                      store
                      projectDir
                      file
                      autoArguments
                      nixPath
                      captureAttrDrvAndEmit
                      uploadDrvInfos
                      sync
                      task
                      allowedPaths
              evalProc `finally` do
                -- Always upload drv infos, even in case of a crash in the worker
                TraversalQueue.waitUntilDone derivationQueue
                  `finally` TraversalQueue.close derivationQueue
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
              EvaluateEvent.AttributeEffect ae -> do
                storePath <- liftIO $ parseStorePath store (encodeUtf8 $ AttributeEffectEvent.derivationPath ae)
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

checkNonEmptyText :: Text -> Maybe Text
checkNonEmptyText "" = Nothing
checkNonEmptyText t = Just t

runEvalProcess ::
  CNix.Store ->
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
  EvaluateTask.EvaluateTask ->
  [ByteString] ->
  App ()
runEvalProcess store projectDir file autoArguments nixPath emit uploadDerivationInfos flush task allowedPaths = do
  extraOpts <- Nix.askExtraOptions
  bulkBaseURL <- asks (ServiceInfo.bulkSocketBaseURL . Env.serviceInfo)
  apiBaseUrl <- asks (toS . showBaseUrl . Env.herculesBaseUrl)
  cfg <- asks Env.config
  srcInput <- getSrcInput task
  gitSource <-
    case srcInput of
      Just git ->
        pure $
          GitSource.GitSource
            { outPath = toS projectDir,
              ref = ImmutableGitInput.ref git,
              rev = ImmutableGitInput.rev git,
              shortRev = GitSource.shortRevFromRev (ImmutableGitInput.rev git),
              branch = GitSource.branchFromRef (ImmutableGitInput.ref git),
              tag = GitSource.tagFromRef (ImmutableGitInput.ref git),
              remoteHttpUrl = ImmutableGitInput.httpURL git & checkNonEmptyText,
              remoteSshUrl = ImmutableGitInput.sshURL git & checkNonEmptyText,
              webUrl = ImmutableGitInput.webURL git,
              forgeType = ImmutableGitInput.forgeType git,
              owner = ImmutableGitInput.owner git,
              name = ImmutableGitInput.name git
            }
      Nothing -> do
        (ref, rev) <- case M.lookup "src" (EvaluateTask.inputMetadata task) of
          Nothing -> do
            panic $ "No primary source metadata provided" <> show task
          Just meta -> pure $ fromMaybe (panic "no ref/rev in primary source metadata") do
            (,)
              <$> (meta ^? at "ref" . traverse . _String)
              <*> (meta ^? at "rev" . traverse . _String)
        pure $
          GitSource.GitSource
            { outPath = toS projectDir,
              ref = ref,
              rev = rev,
              shortRev = GitSource.shortRevFromRev rev,
              branch = GitSource.branchFromRef ref,
              tag = GitSource.tagFromRef ref,
              remoteHttpUrl = Nothing,
              remoteSshUrl = Nothing,
              webUrl = Nothing,
              forgeType = Nothing,
              owner = Nothing,
              name = Nothing
            }
  let eval =
        Eval.Eval
          { Eval.cwd = projectDir,
            Eval.file = toS file,
            Eval.autoArguments = autoArguments,
            Eval.extraNixOptions = extraOpts,
            Eval.logSettings =
              LogSettings.LogSettings
                { token = Sensitive $ EvaluateTask.logToken task,
                  path = "/api/v1/logs/build/socket",
                  baseURL = toS $ Network.URI.uriToString identity bulkBaseURL ""
                },
            Eval.gitSource = ViaJSON gitSource,
            Eval.srcInput = ViaJSON <$> srcInput,
            Eval.apiBaseUrl = apiBaseUrl,
            Eval.ciSystems = EvaluateTask.ciSystems task,
            Eval.selector = ViaJSON $ EvaluateTask.selector task,
            Eval.isFlakeJob = EvaluateTask.isFlakeJob task,
            Eval.allowInsecureBuiltinFetchers = Config.allowInsecureBuiltinFetchers cfg,
            Eval.allowedPaths = allowedPaths
          }
  buildRequiredIndex <- liftIO $ newIORef (0 :: Int)
  attributeIFDCounter <- liftIO $ newIORef (0 :: Int)
  commandChan <- newChan
  writeChan commandChan $ Just $ Command.Eval eval
  for_ (EvaluateTask.extraGitCredentials task) \creds ->
    Netrc.appendLines (credentialToLines =<< creds)
  netrcFile <- Netrc.getNetrcFile
  let decode = decodeUtf8With lenientDecode
      toGitConfigEnv items =
        M.fromList $
          ("GIT_CONFIG_COUNT", show (length items))
            : concatMap
              ( \(i, (k, v)) ->
                  [ ("GIT_CONFIG_KEY_" <> show i, k),
                    ("GIT_CONFIG_VALUE_" <> show i, v)
                  ]
              )
              (zip [0 :: Int ..] items)
      envSettings =
        WorkerProcess.WorkerEnvSettings
          { nixPath = nixPath,
            extraEnv =
              toGitConfigEnv
                [ ("credential.helper", "netrc --file " <> netrcFile),
                  -- Deny by default.
                  ("protocol.allow", "never"),
                  -- Safe protocols.
                  -- More protocols can be added as long as they can be shown
                  -- not to leak credentials from netrc or .git-credentials.
                  -- If a protocol is lacking in confidentiality, authenticity,
                  -- etc, it must be off by default with a Config item to
                  -- enable it.
                  ("protocol.https.allow", "always"),
                  ("protocol.ssh.allow", "always"),
                  ("protocol.file.allow", "always")
                ]
          }
  withProducer (produceWorkerEvents (EvaluateTask.id task) eval envSettings commandChan) $
    \workerEventsP -> fix $ \continue ->
      joinSTM $
        listen
          workerEventsP
          ( \case
              Event.Attribute a | WorkerAttribute.typ a == WorkerAttribute.Effect -> do
                let drvPath = WorkerAttribute.drv a
                secretsMay <- liftIO $ Safe.try do
                  drvStorePath <- CNix.parseStorePath store drvPath
                  derivation <- CNix.getDerivation store drvStorePath
                  drvEnv <- CNix.getDerivationEnv derivation
                  pure $ parseDrvSecretsMap drvEnv

                let emitError msg =
                      emit $
                        EvaluateEvent.AttributeError $
                          AttributeErrorEvent.AttributeErrorEvent
                            { AttributeErrorEvent.expressionPath = decode <$> WorkerAttribute.path a,
                              AttributeErrorEvent.errorMessage = msg,
                              AttributeErrorEvent.errorType = Nothing,
                              AttributeErrorEvent.errorDerivation = Just $ decodeUtf8With lenientDecode (WorkerAttribute.drv a),
                              AttributeErrorEvent.trace = Nothing
                            }
                case secretsMay of
                  Right (Right secrets) -> do
                    emit $
                      EvaluateEvent.AttributeEffect $
                        AttributeEffectEvent.AttributeEffectEvent
                          { expressionPath = decode <$> WorkerAttribute.path a,
                            derivationPath = decode $ WorkerAttribute.drv a,
                            secretsToUse = secrets
                          }
                  Right (Left userError) -> do
                    emitError userError
                  Left technicalError -> do
                    katipAddContext (sl "message" (displayException (technicalError :: SomeException))) do
                      logLocM ErrorS "An unexpected exception occurred while reading an effect."
                    emitError "An unexpected exception occurred while reading the effect. The error message has been logged locally on the agent."
                continue
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
                        AttributeErrorEvent.errorDerivation = WorkerAttributeError.errorDerivation e,
                        AttributeErrorEvent.trace = WorkerAttributeError.trace e
                      }
                continue
              Event.AttributeIFD e -> do
                index <- atomicModifyIORef' attributeIFDCounter \n -> (n + 1, n)
                emit $
                  EvaluateEvent.AttributeIFD $
                    AttributeIFDEvent.AttributeIFDEvent
                      { AttributeIFDEvent.expressionPath = decode <$> AttributeIFD.path e,
                        AttributeIFDEvent.derivationPath = decode $ AttributeIFD.derivationPath e,
                        AttributeIFDEvent.derivationOutput = decode $ AttributeIFD.derivationOutput e,
                        AttributeIFDEvent.done = AttributeIFD.done e,
                        AttributeIFDEvent.index = index
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
              Event.Build drv outputName notAttempt waitForStatus -> do
                storePath <- liftIO (parseStorePath store drv)
                let drvText = decode drv
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
                  let doPoll = do
                        status <- drvPoller notAttempt drvText
                        logLocM DebugS $ "Got derivation status " <> logStr (show status :: Text)
                        writeChan commandChan $ Just $ Command.BuildResult $ uncurry (BuildResult.BuildResult drvText) status
                  if waitForStatus
                    then do
                      flush
                      doPoll
                    else void $ do
                      uio <- askUnliftIO
                      liftIO $ forkIO $ unliftIO uio doPoll
                continue
              Event.OnPushHandler (ViaJSON e) -> do
                emit $ EvaluateEvent.OnPushHandlerEvent e
                continue
              Event.OnScheduleHandler (ViaJSON e) -> do
                emit $ EvaluateEvent.OnScheduleHandlerEvent e
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

credentialToLines :: EvaluateTask.Credential -> [Text]
credentialToLines c =
  fromMaybe [] do
    host <- hostFromUrl (EvaluateTask.url c)
    pure
      [ "machine " <> host,
        "login " <> EvaluateTask.username c,
        "password " <> EvaluateTask.password c
      ]

hostFromUrl :: Text -> Maybe Text
hostFromUrl t = do
  uri <- Network.URI.parseURI (toS t)
  a <- Network.URI.uriAuthority uri
  pure $ toS $ Network.URI.uriRegName a

produceWorkerEvents ::
  Id (Task a) ->
  Eval.Eval ->
  WorkerProcess.WorkerEnvSettings ->
  Chan (Maybe Command.Command) ->
  (Event.Event -> App ()) ->
  App ExitCode
produceWorkerEvents taskId eval envSettings commandChan writeEvent = do
  workerExe <- WorkerProcess.getWorkerExe
  let opts = [show $ Eval.extraNixOptions eval]
  -- NiceToHave: replace renderNixPath by something structured like -I
  -- to support = and : in paths
  workerEnv <- liftIO $ WorkerProcess.prepareEnv envSettings
  let wps =
        (System.Process.proc workerExe opts)
          { env = Just workerEnv,
            close_fds = True, -- Disable on Windows?
            cwd = Just (Eval.cwd eval)
          }
      stderrHandler =
        stderrLineHandler
          ( M.fromList
              [ ("taskId", A.toJSON taskId),
                ("evalRev", A.toJSON (eval & Eval.gitSource & fromViaJSON & GitSource.rev))
              ]
          )
          "Effect worker"
  WorkerProcess.runWorker wps stderrHandler commandChan writeEvent

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
