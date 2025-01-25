{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Agent.Evaluate
  ( performEvaluation,
  )
where

import Conduit
import Control.Concurrent.Async (waitSTM)
import Control.Concurrent.Async.Lifted qualified as Async.Lifted
import Control.Concurrent.Chan.Lifted
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
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
  )
import Data.Map qualified as M
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.Vector (Vector)
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
import Hercules.API.Agent.Evaluate.EvaluateEvent.SubstitutionQueryResult qualified as SubstitutionQueryResult
import Hercules.API.Agent.Evaluate.EvaluateTask qualified as EvaluateTask
import Hercules.API.Agent.Evaluate.ImmutableGitInput (ImmutableGitInput)
import Hercules.API.Agent.Evaluate.ImmutableGitInput qualified as ImmutableGitInput
import Hercules.API.Agent.Evaluate.ImmutableInput qualified as ImmutableInput
import Hercules.API.Logs.LogEntry (LogEntry)
import Hercules.API.Servant (noContent)
import Hercules.API.Task (Task)
import Hercules.Agent.Build (convertOutputInfo)
import Hercules.Agent.Cache (getConfiguredSubstituters)
import Hercules.Agent.Cache qualified as Agent.Cache
import Hercules.Agent.Client qualified
import Hercules.Agent.Config qualified as Config
import Hercules.Agent.Env
import Hercules.Agent.Env qualified as Env
import Hercules.Agent.Files
import Hercules.Agent.InitWorkerConfig qualified as WorkerConfig
import Hercules.Agent.Log
import Hercules.Agent.Memo (Memo, doOnce, newMemo)
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
import Hercules.Agent.ResourceLimiter (ResourceLimiter, newResourceLimiter, withResource)
import Hercules.Agent.Store (toDrvInfo)
import Hercules.Agent.WorkerProcess qualified as WorkerProcess
import Hercules.Agent.WorkerProtocol.Command qualified as Command
import Hercules.Agent.WorkerProtocol.Command.BuildResult qualified as BuildResult
import Hercules.Agent.WorkerProtocol.Command.Eval qualified as Eval
import Hercules.Agent.WorkerProtocol.Event qualified as Event
import Hercules.Agent.WorkerProtocol.Event.Attribute qualified as WorkerAttribute
import Hercules.Agent.WorkerProtocol.Event.AttributeError qualified as WorkerAttributeError
import Hercules.Agent.WorkerProtocol.Event.AttributeIFD qualified as AttributeIFD
import Hercules.Agent.WorkerProtocol.ViaJSON (ViaJSON (ViaJSON))
import Hercules.Agent.WorkerProtocol.ViaJSON qualified
import Hercules.CNix.Store (Store, StorePath, getStorePathBaseName, parseStorePath)
import Hercules.CNix.Store qualified as CNix
import Hercules.Effect (parseDrvSecretsMap)
import Hercules.Error (defaultRetry, quickRetry)
import Network.HTTP.Client.Conduit qualified as HTTP.Conduit
import Network.HTTP.Simple qualified as HTTP.Simple
import Network.URI qualified
import Protolude hiding (async, atomically, concurrently, finally, newChan, writeChan)
import Servant.Client qualified
import Servant.Client.Core (showBaseUrl)
import System.Directory qualified as Dir
import System.FilePath
import System.Process
import UnliftIO (async, atomicModifyIORef', atomically, concurrently, concurrently_, forConcurrently, forConcurrently_, modifyTVar, readTVar, writeTVar)
import UnliftIO qualified

eventLimit :: Int
eventLimit = 50000

pushEvalWorkers :: Int
pushEvalWorkers = 16

performEvaluation :: (Vector LogEntry -> IO ()) -> Store -> EvaluateTask.EvaluateTask -> App ()
performEvaluation sendLogItems store task' =
  withProducer (produceEvaluationTaskEvents sendLogItems store task') $ \producer ->
    withBoundedDelayBatchProducer (1000 * 1000) 1000 producer $ \batchProducer ->
      fix $ \continue ->
        joinSTM $ listen batchProducer (\b -> withSync b (postBatch task' . catMaybes) *> continue) pure

getSrcInput :: (MonadIO m) => EvaluateTask.EvaluateTask -> m (Maybe ImmutableGitInput)
getSrcInput task = case M.lookup "src" (EvaluateTask.inputs task) of
  Just (ImmutableInput.Git x) ->
    purer x
  Nothing -> do
    throwIO $ FatalError $ "No src input provided" <> show task
  Just {} -> do
    pure Nothing

data AbortMessageAlreadySent = AbortMessageAlreadySent deriving (Show, Exception)

getWithStoreLimiter :: (MonadUnliftIO m) => (Env -> Memo Text ResourceLimiter) -> Text -> App (m a -> m a)
getWithStoreLimiter getLimiterMap store = do
  memo <- asks getLimiterMap
  limiter <- doOnce memo store do
    newResourceLimiter 32
  pure (withResource limiter)

-- | Apply concurreny limit
withStoreQuery :: Text -> App a -> App a
withStoreQuery storeURI m = do
  withLimit <- getWithStoreLimiter Env.concurrentStoreQueries storeURI
  withLimit m

-- | Apply concurreny limit
withStorePush :: Text -> App a -> App a
withStorePush storeURI m = do
  withLimit <- getWithStoreLimiter Env.concurrentStorePushes storeURI
  withLimit m

makeEventEmitter ::
  (Syncing EvaluateEvent.EvaluateEvent -> App ()) ->
  App (EvaluateEvent.EvaluateEvent -> App ())
makeEventEmitter writeToBatch = do
  let emitSingle = writeToBatch . Syncable

  eventCounter <- liftIO $ newIORef 0
  msgCounter <- liftIO $ newIORef 0

  let fixIndex ::
        (MonadIO m) =>
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

-- | @withDynamicBarrier (\addWait -> m ...)@ waits for any number of concurrent operations,
-- which are registered during the execution of @m@ using @addWait@.
--
-- The @addWait@ function enqueues an 'STM' transaction that will be dequeued only when the transaction completes successfully.
withDynamicBarrier :: (MonadUnliftIO m) => ((STM x -> STM ()) -> m a) -> m a
withDynamicBarrier driver = do
  -- Signals that `driver` is done. Prevents exiting before work builds up.
  driverDone :: TVar (STM ()) <- liftIO (newTVarIO retry)

  -- Collection of work that needs to complete before returning, including the `driver` itself.
  -- It's a list so that wait conditions that have completed can be removed; not checked again.
  done :: TVar [STM ()] <- liftIO (newTVarIO [join $ readTVar driverDone])

  let blockOn :: STM a -> STM ()
      blockOn stm = do
        modifyTVar done (void stm :)

      waitDone =
        atomically isDone >>= \case
          True -> pass
          False -> waitDone

      isDone :: STM Bool
      isDone = do
        readTVar done >>= \case
          [] -> pure True
          (c : cs) -> do
            c
            writeTVar done cs
            pure False

      claimDriverDone = writeTVar driverDone pass

  fst <$> concurrently (driver blockOn `UnliftIO.finally` atomically claimDriverDone) waitDone

withStores :: [Text] -> (Map Text Store -> App r) -> App r
withStores storeURIs f =
  let sorted = ordNub storeURIs
   in foldr
        ( \uri f2 stores -> do
            CNix.withStoreFromURI uri \store ->
              f2 (stores <> M.singleton uri store)
        )
        f
        sorted
        mempty

withSubstituters :: (Map Text Store -> App r) -> App r
withSubstituters f = do
  substituterURIs <- getConfiguredSubstituters
  withStores substituterURIs f

produceEvaluationTaskEvents ::
  (Vector LogEntry -> IO ()) ->
  Store ->
  EvaluateTask.EvaluateTask ->
  (Syncing EvaluateEvent.EvaluateEvent -> App ()) ->
  App ()
produceEvaluationTaskEvents sendLogItems store task writeToBatch = UnliftIO.handle (\AbortMessageAlreadySent -> pass) $ withSubstituters \substituters -> withWorkDir "eval" $ \tmpdir -> do
  let sync = syncer writeToBatch
  emit <- makeEventEmitter writeToBatch

  derivationInfoUpload :: Memo ByteString () <- newMemo
  derivationSubstitutable :: Memo (Text, ByteString) Bool <- newMemo
  derivationBuildRequest :: Memo Text () <- newMemo
  derivationCache :: Memo ByteString CNix.Derivation <- newMemo
  planBuild_ <- newMemo

  let emitDrvInfoRaw :: StorePath -> App ()
      emitDrvInfoRaw drvPath = do
        drvInfo <- retrieveDerivationInfo store drvPath
        forConcurrently_ (M.keys $ DerivationInfo.inputDerivations drvInfo) \inp -> do
          inputStorePath <- liftIO $ parseStorePath store (encodeUtf8 inp)
          emitDrvInfo inputStorePath
        emit $ EvaluateEvent.DerivationInfo drvInfo

      getDerivationCached :: StorePath -> App CNix.Derivation
      getDerivationCached drvPath = do
        bs <- liftIO do getStorePathBaseName drvPath
        doOnce derivationCache bs do
          liftIO $ CNix.getDerivation store drvPath

      emitDrvInfo :: StorePath -> App ()
      emitDrvInfo sp = do
        bs <- liftIO (CNix.getStorePathBaseName sp)
        doOnce derivationInfoUpload bs do
          emitDrvInfoRaw sp

      rawQuerySubstitutableOutput :: StorePath -> CNix.Derivation -> Text -> ByteString -> App Bool
      rawQuerySubstitutableOutput drvPath drv drvPathText outputName = do
        drvName <- liftIO $ CNix.getDerivationNameFromPath drvPath
        outputs <- liftIO $ CNix.getDerivationOutputs store drvName drv
        output <- case find (\o -> o.derivationOutputName == outputName) outputs of
          Nothing -> panic $ "derivation " <> drvPathText <> " does not have output " <> show outputName
          Just x -> pure x
        querySubstitutableOutput' drvPathText outputName output

      querySubstitutableOutput' drvPathText outputName output = do
        doOnce derivationSubstitutable (drvPathText, outputName) do
          rawQuerySubstitutableOutput' drvPathText outputName output

      rawQuerySubstitutableOutput' drvPathText outputName output = do
        outputPath <- case output.derivationOutputPath of
          Nothing -> panic $ "derivation " <> drvPathText <> " does not have a predetermined path. Content addressed derivations are not supported yet."
          Just x -> pure x
        alreadyPositive <-
          any (isJust . join) <$> for (M.toList substituters) \(_uri, substituter) -> liftIO do
            CNix.queryPathInfoFromClientCache substituter outputPath
        if alreadyPositive
          then pure True
          else do
            -- TODO waitAny
            any identity <$> forConcurrently (M.toList substituters) \(uri, substituter) -> do
              exists <- withStoreQuery uri do
                liftIO (CNix.isValidPath substituter outputPath)
              if exists
                then do
                  drvInfo <- liftIO (toDrvInfo substituter output)
                  emit $
                    EvaluateEvent.SubstitutionQueryResult
                      SubstitutionQueryResult.SubstitutionQueryResult
                        { storeURI = uri,
                          derivation = drvPathText,
                          outputName = decodeUtf8With lenientDecode outputName,
                          outputInfo = Just (convertOutputInfo drvPathText drvInfo)
                        }
                else do
                  emit $
                    EvaluateEvent.SubstitutionQueryResult
                      SubstitutionQueryResult.SubstitutionQueryResult
                        { storeURI = uri,
                          derivation = drvPathText,
                          outputName = decodeUtf8With lenientDecode outputName,
                          outputInfo = Nothing
                        }

              pure exists

      storePathToText :: StorePath -> App Text
      storePathToText sp = do
        liftIO (CNix.storePathToPath store sp) <&> decodeUtf8With lenientDecode

      planAllOutputs :: StorePath -> App ()
      planAllOutputs drvPath = do
        drvPathText <- storePathToText drvPath
        drv <- getDerivationCached drvPath
        drvName <- liftIO $ CNix.getDerivationNameFromPath drvPath
        outputs <- liftIO $ CNix.getDerivationOutputs store drvName drv
        outputsSubstitutable <-
          all identity <$> forConcurrently outputs \output ->
            querySubstitutableOutput' drvPathText output.derivationOutputName output

        when (not outputsSubstitutable) do
          planBuild drv drvPathText

      -- Like 'planInputs' but do assume we need to build all of them.
      planInputsForced drvPath = do
        drv <- getDerivationCached drvPath
        inputs <- liftIO (CNix.getDerivationInputs' drv)
        for_ inputs \(inputDrvPath, _outputs) -> do
          inputDrv <- getDerivationCached inputDrvPath
          planBuild inputDrv =<< storePathToText inputDrvPath

      planInputs drv = do
        inputs <- liftIO (CNix.getDerivationInputs' drv)
        for_ inputs \(inputDrvPath, outputs) -> do
          inputDrvPathText <- decodeUtf8With lenientDecode <$> liftIO (CNix.storePathToPath store inputDrvPath)
          inputDrv <- getDerivationCached inputDrvPath
          for outputs \outputName -> do
            planOutput inputDrvPath inputDrv outputName inputDrvPathText

      planOutput :: StorePath -> CNix.Derivation -> ByteString -> Text -> App ()
      planOutput drvPath drv outputName drvPathText = do
        isSubstitutable <- rawQuerySubstitutableOutput drvPath drv drvPathText outputName
        if isSubstitutable
          then pass
          else do
            planBuild drv drvPathText

      -- Plan a build, when it has been determined that a build task is necessary.
      planBuild drv drvPathText =
        doOnce planBuild_ drvPathText do
          planInputs drv
          requestBuild drvPathText

      requestBuild drvPathText =
        doOnce derivationBuildRequest drvPathText do
          emit $ EvaluateEvent.BuildRequest BuildRequest.BuildRequest {derivationPath = drvPathText, forceRebuild = False}

  let isFlakeJob = EvaluateTask.isFlakeJob task
  nonFlakeStuff <- for (guard (not isFlakeJob)) \() -> do
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
    adHocSystem <-
      readFileMaybe (projectDir </> "ci-default-system.txt")

    file <-
      liftIO (findNixFile projectDir) >>= \case
        Left e -> do
          emit $
            EvaluateEvent.Message
              Message.Message
                { Message.index = -1, -- will be set by emit
                  Message.typ = Message.Error,
                  Message.message = e
                }
          throwIO AbortMessageAlreadySent
        Right file ->
          pure file

    pure (nixPath, autoArguments, inputLocations, adHocSystem, projectDir, file)

  let nixPath = case nonFlakeStuff of
        Nothing -> mempty
        Just (np, _, _, _, _, _) -> np
      autoArgs = case nonFlakeStuff of
        Nothing -> mempty
        Just (_, aa, _, _, _, _) -> aa
      allowedPaths = case nonFlakeStuff of
        Nothing -> []
        Just (_, _, inputLocations, _, _, _) -> toList inputLocations <&> toS <&> encodeUtf8
      adHocSystem = case nonFlakeStuff of
        Nothing -> Nothing
        Just (_, _, _, a, _, _) -> a
      projectDir = case nonFlakeStuff of
        Nothing -> tmpdir -- unused
        Just (_, _, _, _, pd, _) -> pd
      file = case nonFlakeStuff of
        Nothing -> "" -- unused
        Just (_, _, _, _, _, file_) -> file_

  let uploadDrvs paths = do
        caches <-
          case EvaluateTask.pushToBinaryCaches task of
            Nothing -> activePushCaches
            Just desiredPushCaches -> do
              availablePushCaches <- activePushCaches
              pure . M.keys $ desiredPushCaches `M.intersection` (M.fromList $ (,()) <$> availablePushCaches)
        forM_ caches $ \cache -> do
          withNamedContext "cache" cache $ logLocM DebugS "Pushing derivations"

          -- TODO: Make it fine grained? withStorePush here and now is a limit on concurrent closures.
          withStorePush cache do
            Agent.Cache.push store cache (toList paths) pushEvalWorkers

  withDynamicBarrier \addToWait ->
    let addAsync = addToWait . waitSTM
        uploadDrvInfos drvPath = do
          emitDrvInfo drvPath
          uploadDrvs [drvPath]
        addTopDerivation drvPath = do
          atomically . addAsync =<< async do
            concurrently_
              (emitDrvInfo drvPath)
              (uploadDrvs [drvPath])
            planAllOutputs drvPath
            -- In theory we should be done now
            -- but we want derivation events for these for the backend, and
            -- potentially for inspecting the nix-support directory (TBD).
            -- FIXME: this realises build inputs even if output was substitutable
            --        (but only for top level derivations)
            planInputs =<< getDerivationCached drvPath
            requestBuild =<< storePathToText drvPath

        addTopDerivationInputs drvPath = do
          atomically . addAsync =<< async do
            concurrently_
              (emitDrvInfo drvPath)
              (uploadDrvs [drvPath])
            -- In theory we should be done (as in addTopDerivation) but we want
            -- derivation events for the inputs, so we force them.
            -- FIXME: this realises build inputs even if output was substitutable
            --        (but only for top level deps-only derivations' inputs)
            planInputsForced drvPath

        extraOpts = [("system", T.strip s) | Just s <- [adHocSystem]]
        evaluation = do
          let evalProc =
                Nix.withExtraOptions extraOpts do
                  runEvalProcess
                    sendLogItems
                    store
                    projectDir
                    file
                    autoArgs
                    nixPath
                    captureAttrDrvAndEmit
                    uploadDrvInfos
                    sync
                    task
                    allowedPaths
          evalProc
        captureAttrDrvAndEmit msg = do
          case msg of
            EvaluateEvent.Attribute ae -> do
              storePath <- liftIO $ parseStorePath store (encodeUtf8 $ AttributeEvent.derivationPath ae)
              case ae.typ of
                AttributeEvent.Regular -> addTopDerivation storePath
                AttributeEvent.MustFail -> addTopDerivation storePath
                AttributeEvent.MayFail -> addTopDerivation storePath
                AttributeEvent.DependenciesOnly -> addTopDerivationInputs storePath
                AttributeEvent.Effect -> addTopDerivationInputs storePath
            EvaluateEvent.AttributeEffect ae -> do
              storePath <- liftIO $ parseStorePath store (encodeUtf8 $ AttributeEffectEvent.derivationPath ae)
              addTopDerivationInputs storePath
            _ -> pass
          emit msg
     in evaluation

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
  (Vector LogEntry -> IO ()) ->
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
runEvalProcess sendLogItems store projectDir file autoArguments nixPath emit uploadDerivationInfos flush task allowedPaths = do
  extraOpts <- Nix.askExtraOptions
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
            Eval.gitSource = ViaJSON gitSource,
            Eval.srcInput = ViaJSON <$> srcInput,
            Eval.apiBaseUrl = apiBaseUrl,
            Eval.ciSystems = EvaluateTask.ciSystems task,
            Eval.pushToBinaryCaches = EvaluateTask.pushToBinaryCaches task,
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
  withProducer (produceWorkerEvents sendLogItems (EvaluateTask.id task) eval envSettings commandChan) $
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
                          withStorePush cache do
                            Agent.Cache.push store cache [storePath] pushEvalWorkers
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
              Event.LogItems (ViaJSON e) -> do
                liftIO (sendLogItems e)
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
  (Vector LogEntry -> IO ()) ->
  Id (Task EvaluateTask.EvaluateTask) ->
  Eval.Eval ->
  WorkerProcess.WorkerEnvSettings ->
  Chan (Maybe Command.Command) ->
  (Event.Event -> App ()) ->
  App ExitCode
produceWorkerEvents sendLogEntries taskId eval envSettings commandChan writeEvent = do
  workerExe <- WorkerProcess.getWorkerExe
  let opts = ["eval", fromMaybe "" eval.gitSource.fromViaJSON.webUrl, eval.gitSource.fromViaJSON.rev] <&> T.unpack
  workerEnv <- liftIO $ WorkerProcess.prepareEnv envSettings
  let wps =
        (System.Process.proc workerExe opts)
          { env = Just workerEnv,
            close_fds = True, -- Disable on Windows?
            cwd = Just (Eval.cwd eval)
          }
      stderrHandler =
        stderrLineHandler
          sendLogEntries
          ( M.fromList
              [ ("taskId", A.toJSON taskId),
                ("evalRev", A.toJSON (eval.gitSource.fromViaJSON.rev))
              ]
          )
          "Effect worker"
  cfg <- WorkerConfig.getWorkerConfig
  WorkerProcess.runWorker cfg wps stderrHandler commandChan writeEvent

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
  deriving (Typeable, Show)
  deriving anyclass (Exception)

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
