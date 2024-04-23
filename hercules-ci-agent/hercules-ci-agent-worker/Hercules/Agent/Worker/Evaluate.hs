{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Hercules.Agent.Worker.Evaluate where

import Conduit
import Control.Concurrent.Async (pollSTM)
import Control.Concurrent.STM hiding (check)
import Control.Exception.Safe (isAsyncException)
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Data.Coerce (coerce)
import Data.Conduit qualified
import Data.Conduit.Katip.Orphans ()
import Data.IORef
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Type.Equality (type (~))
import Hercules.API.Agent.Evaluate.EvaluateEvent.OnPushHandlerEvent (OnPushHandlerEvent (OnPushHandlerEvent))
import Hercules.API.Agent.Evaluate.EvaluateEvent.OnPushHandlerEvent qualified
import Hercules.API.Agent.Evaluate.EvaluateEvent.OnScheduleHandlerEvent (OnScheduleHandlerEvent (OnScheduleHandlerEvent), TimeConstraints (TimeConstraints))
import Hercules.API.Agent.Evaluate.EvaluateEvent.OnScheduleHandlerEvent qualified
import Hercules.API.Agent.Evaluate.EvaluateTask qualified as EvaluateTask
import Hercules.API.Agent.Evaluate.EvaluateTask.OnPush qualified as OnPush
import Hercules.API.Agent.Evaluate.EvaluateTask.OnSchedule qualified as OnSchedule
import Hercules.API.Agent.Evaluate.ImmutableGitInput qualified as API.ImmutableGitInput
import Hercules.API.Agent.Evaluate.ImmutableInput qualified as API.ImmutableInput
import Hercules.API.DayOfWeek (DayOfWeek (..))
import Hercules.Agent.NixFile (HerculesCISchema, InputsSchema, OutputsSchema, getHerculesCI, homeExprRawValue, loadNixFile, parseExtraInputs)
import Hercules.Agent.NixFile qualified as NixFile
import Hercules.Agent.NixFile.HerculesCIArgs (BinaryCaches (BinaryCaches), CISystems (CISystems), HerculesCIMeta (HerculesCIMeta), fromGitSource)
import Hercules.Agent.NixFile.HerculesCIArgs qualified
import Hercules.Agent.Worker.Env (HerculesState (..))
import Hercules.Agent.Worker.Error (ExceptionText (exceptionTextDerivationPath), exceptionTextMessage, exceptionTextTrace, renderException, throwBuildError)
import Hercules.Agent.Worker.HerculesStore (nixStore, setBuilderCallback)
import Hercules.Agent.Worker.STM (asyncInTVarMap)
import Hercules.Agent.WorkerProtocol.Command.BuildResult qualified as BuildResult
import Hercules.Agent.WorkerProtocol.Command.Eval
  ( Eval,
  )
import Hercules.Agent.WorkerProtocol.Command.Eval qualified as Eval
import Hercules.Agent.WorkerProtocol.Event (Event)
import Hercules.Agent.WorkerProtocol.Event qualified as Event
import Hercules.Agent.WorkerProtocol.Event.Attribute qualified as Attribute
import Hercules.Agent.WorkerProtocol.Event.AttributeError qualified as AttributeError
import Hercules.Agent.WorkerProtocol.Event.AttributeIFD qualified as Event.AttributeIFD
import Hercules.Agent.WorkerProtocol.ViaJSON qualified as ViaJSON
import Hercules.CNix as CNix
import Hercules.CNix.Expr (Match (IsAttrs, IsString), NixAttrs, RawValue, addAllowedPath, addInternalAllowedPaths, autoCallFunction, evalArgs, getAttrBool, getAttrList, getAttrs, getDrvFile, getFlakeFromArchiveUrl, getFlakeFromGit, getRecurseForDerivations, getStringIgnoreContext, isDerivation, isFunctor, match, rawValueType, rtValue, toRawValue, toValue, withEvalStateConduit)
import Hercules.CNix.Expr.Context (EvalState)
import Hercules.CNix.Expr.Raw qualified
import Hercules.CNix.Expr.Schema (MonadEval, PSObject, dictionaryToMap, fromPSObject, provenance, requireDict, traverseArray, (#.), (#?), (#?!), (#??), ($?), (|!), type (->?), type (.))
import Hercules.CNix.Expr.Schema qualified as Schema
import Hercules.CNix.Expr.Typed (Value)
import Hercules.CNix.Std.Vector (StdVector)
import Hercules.CNix.Std.Vector qualified as Std.Vector
import Hercules.CNix.Store.Context (NixStorePathWithOutputs)
import Hercules.Error
import Hercules.UserException (UserException (UserException))
import Katip
import Protolude hiding (bracket, catch, check, evalState, wait, withAsync, yield)
import UnliftIO qualified
import UnliftIO.Async (wait)
import UnliftIO.Exception (bracket, catch)
import Prelude ()

data AsyncRealisationRequired = AsyncRealisationRequired
  { realisationRequiredDerivationPath :: StorePath,
    realisationRequiredOutputName :: ByteString
  }
  deriving (Show, Typeable, Eq, Ord)

instance Exception AsyncRealisationRequired

isAsyncRealisationRequired :: SomeException -> Bool
isAsyncRealisationRequired e = case fromException e of
  Just (_ :: AsyncRealisationRequired) -> True
  Nothing -> False

data EvalEnv = EvalEnv
  { evalEnvHerculesState :: HerculesState,
    evalEnvState :: Ptr EvalState,
    evalEnvIsNonBlocking :: IORef Bool
  }

evalEnvStore :: EvalEnv -> Store
evalEnvStore = nixStore . herculesStore . evalEnvHerculesState

-- TODO: test
autoArgArgs :: Map Text Eval.Arg -> [ByteString]
autoArgArgs kvs = do
  (k, v) <- M.toList kvs
  case v of
    Eval.LiteralArg s -> ["--argstr", encodeUtf8 k, s]
    Eval.ExprArg s -> ["--arg", encodeUtf8 k, s]

-- Ensure that a nested build invocation does not happen by a mistake in wiring.
--
-- (It does not happen, but this de-escalates a potential recursion bug to just
--  an error.)
withDrvInProgress :: (MonadUnliftIO m) => HerculesState -> StorePath -> m a -> m a
withDrvInProgress HerculesState {drvsInProgress = ref} drvPath =
  bracket acquire release . const
  where
    acquire =
      liftIO $
        join $
          atomicModifyIORef ref $ \inprg ->
            if drvPath `S.member` inprg
              then (inprg, throwIO $ FatalError "Refusing to build derivation that should have been built remotely. Presumably, substitution has failed.")
              else (S.insert drvPath inprg, pass)
    release _ =
      liftIO $
        atomicModifyIORef ref $ \inprg ->
          (S.delete drvPath inprg, ())

anyAlternative :: (Foldable l, Alternative f) => l a -> f a
anyAlternative = getAlt . foldMap (Alt . pure)

yieldAttributeError :: (MonadIO m) => Store -> [ByteString] -> SomeException -> ConduitT i Event m ()
yieldAttributeError store path e = do
  exceptionText <- liftIO $ renderException e
  drvPath <- liftIO $ traverse (storePathToPath store) (exceptionTextDerivationPath exceptionText)
  yield $
    Event.AttributeError $
      AttributeError.AttributeError
        { AttributeError.path = path,
          AttributeError.message = exceptionTextMessage exceptionText,
          AttributeError.errorDerivation = drvPath <&> decodeUtf8With lenientDecode,
          AttributeError.errorType = if isJust (exceptionTextDerivationPath exceptionText) then Just "BuildException" else Just (show (typeOf e)),
          AttributeError.trace = exceptionTextTrace exceptionText
        }

maybeThrowBuildException :: (MonadIO m) => BuildResult.BuildStatus -> StorePath -> m ()
maybeThrowBuildException result drv =
  case result of
    BuildResult.Failure -> liftIO $ throwBuildError ("Could not build derivation " <> show drv <> ", which is required during evaluation.") drv
    BuildResult.DependencyFailure -> liftIO $ throwBuildError ("Could not build a dependency of derivation " <> show drv <> ", which is required during evaluation.") drv
    BuildResult.Success -> pass

mkCache :: forall k a m. (MonadUnliftIO m, Ord k) => (SomeException -> Bool) -> IO (k -> m a -> m a)
mkCache cacheableException = do
  doneRef <- liftIO $ newIORef mempty
  let hasBeenBuilt :: k -> m (Maybe (Either SomeException a))
      hasBeenBuilt p = liftIO $ readIORef doneRef <&> \c -> M.lookup p c
      cacheBy p io = do
        b <- hasBeenBuilt p
        case b of
          Just x -> liftIO $ escalate x
          Nothing -> do
            r <- UnliftIO.tryAny io
            liftIO do
              let cacheable = case r of
                    Right {} -> True
                    Left e -> cacheableException e
              when cacheable do
                modifyIORef doneRef (M.insert p (r :: Either SomeException a))
              escalate r
  pure cacheBy

runEval ::
  forall i m.
  (MonadResource m, KatipContext m, MonadUnliftIO m, MonadThrow m) =>
  HerculesState ->
  Eval ->
  ConduitM i Event m ()
runEval st@HerculesState {herculesStore = hStore, drvsCompleted = drvsCompl} eval = do
  -- TODO: redundant as these have already been set?
  for_ (Eval.extraNixOptions eval) $ liftIO . uncurry setGlobalOption
  for_ (Eval.extraNixOptions eval) $ liftIO . uncurry setOption

  let store = nixStore hStore
      isFlake = Eval.isFlakeJob eval
  s <- storeUri store
  UnliftIO unlift <- lift askUnliftIO
  let decode = decodeUtf8With lenientDecode

  cachingBuilt <- liftIO $ mkCache (not . isAsyncRealisationRequired)

  isNonBlocking <- liftIO (newIORef False)

  liftIO . setBuilderCallback hStore $
    traverseSPWOs $ \storePathWithOutputs -> unlift $ do
      drvStorePath <- liftIO $ getStorePath storePathWithOutputs
      drvPath <- liftIO $ CNix.storePathToPath store drvStorePath
      cachingBuilt drvPath do
        let pathText = decode drvPath
        outputs0 <- liftIO $ getOutputs storePathWithOutputs
        derivation <- liftIO $ getDerivation store drvStorePath
        drvName <- liftIO $ getDerivationNameFromPath drvStorePath
        drvOutputs <- liftIO $ getDerivationOutputs store drvName derivation
        let outputs =
              -- Empty outputs suggests that the intent was to build all outputs
              -- Tests do not confirm, but better be sure.
              -- TODO: move away from StorePathWithOutputs; feasible since 2.4?
              if null outputs0
                then map (.derivationOutputName) drvOutputs
                else outputs0
        katipAddContext (sl "fullpath" pathText) $
          for_ outputs $ \outputName -> do
            withDrvInProgress st drvStorePath $ do
              outputPath <-
                case find (\o -> derivationOutputName o == outputName) drvOutputs of
                  Nothing -> panic $ "output " <> show outputName <> " does not exist on " <> pathText
                  Just o -> case derivationOutputPath o of
                    Just x -> pure x
                    Nothing ->
                      -- FIXME ca-derivations
                      panic $ "output path unknown for output " <> show outputName <> " on " <> pathText <> ". ca-derivations is not supported yet."
              liftIO $ addTemporaryRoot store outputPath

              isValid <- liftIO $ isValidPath store outputPath
              if isValid
                then do
                  logLocM DebugS "Output already valid"
                  -- Report IFD
                  liftIO $ st.sendEvents $ pure $ Event.Build drvPath (decode outputName) Nothing False
                else do
                  don'tBlock <- liftIO (readIORef isNonBlocking)
                  let doBlock = not don'tBlock

                  liftIO $ st.sendEvents $ pure $ Event.Build drvPath (decode outputName) Nothing doBlock

                  buildAsync <- liftIO $ asyncInTVarMap (drvStorePath, outputName) (drvOutputSubstituteAsyncs st) do
                    ensurePath (wrappedStore st) outputPath
                      `catch` \(_e :: SomeException) -> do
                        -- wait for build that was requested before

                        (attempt0, result) <- wait <=< liftIO $ asyncInTVarMap drvStorePath (drvBuildAsyncs st) do
                          atomically $ do
                            c <- readTVar drvsCompl
                            anyAlternative $ M.lookup drvStorePath c

                        maybeThrowBuildException result drvStorePath
                        clearSubstituterCaches
                        clearPathInfoCache store
                        ensurePath (wrappedStore st) outputPath `catch` \(_e1 :: SomeException) -> do
                          st.sendEvents $ pure $ Event.Build drvPath (decode outputName) (Just attempt0) doBlock

                          (_, result') <-
                            wait =<< asyncInTVarMap drvStorePath (drvRebuildAsyncs st) do
                              atomically $ do
                                c <- readTVar drvsCompl
                                (attempt1, r) <- anyAlternative $ M.lookup drvStorePath c
                                guard (attempt1 /= attempt0)
                                pure (attempt1, r)

                          maybeThrowBuildException result' drvStorePath
                          clearSubstituterCaches
                          clearPathInfoCache store
                          ensurePath (wrappedStore st) outputPath `catch` \e2 ->
                            liftIO $
                              throwBuildError
                                ( "Build failure: output could not be retrieved on the evaluating agent, despite a successful rebuild. Exception: "
                                    <> show (e2 :: SomeException)
                                )
                                drvStorePath
                    pure outputPath

                  maybeAlreadyDone <- liftIO $ poll buildAsync
                  case maybeAlreadyDone of
                    Just r -> void $ escalate r
                    Nothing -> do
                      if don'tBlock
                        then throwIO (AsyncRealisationRequired drvStorePath outputName)
                        else void $ wait buildAsync

  withEvalStateConduit store $ \evalState -> do
    let evalEnv :: EvalEnv
        evalEnv =
          EvalEnv
            { evalEnvHerculesState = st,
              evalEnvIsNonBlocking = isNonBlocking,
              evalEnvState = evalState
            }

    liftIO do
      addInternalAllowedPaths evalState
      for_ (Eval.allowedPaths eval) (addAllowedPath evalState)
    katipAddContext (sl "storeURI" (decode s)) $
      logLocM DebugS "EvalState loaded."
    args <-
      liftIO $
        evalArgs evalState (autoArgArgs (Eval.autoArguments eval))
    Data.Conduit.handleC (yieldAttributeError store []) $
      do
        homeExpr <- getHomeExpr evalState eval
        let hargs = fromGitSource (coerce $ Eval.gitSource eval) meta
            meta = HerculesCIMeta {apiBaseUrl = Eval.apiBaseUrl eval, ciSystems = CISystems (Eval.ciSystems eval), pushToBinaryCaches = BinaryCaches (Eval.pushToBinaryCaches eval)}
        liftIO (flip runReaderT evalState $ getHerculesCI homeExpr hargs) >>= \case
          Nothing ->
            -- legacy
            walk evalEnv args (homeExprRawValue homeExpr)
          Just herculesCI -> do
            case ViaJSON.fromViaJSON (Eval.selector eval) of
              EvaluateTask.ConfigOrLegacy -> do
                yield Event.JobConfig
                sendConfig evalState isFlake herculesCI
              EvaluateTask.OnPush onPush ->
                transPipe (`runReaderT` evalState) do
                  walkOnPush evalEnv onPush herculesCI
              EvaluateTask.OnSchedule onSchedule ->
                transPipe (`runReaderT` evalState) do
                  walkOnSchedule evalEnv onSchedule herculesCI
    yield Event.EvaluationDone

getHomeExpr :: (MonadThrow m, MonadIO m) => Ptr EvalState -> Eval -> m NixFile.HomeExpr
getHomeExpr evalState eval =
  if Eval.isFlakeJob eval
    then
      NixFile.Flake <$> liftIO do
        srcInput <- case Eval.srcInput eval of
          Just x -> pure x
          Nothing -> panic "srcInput is required for flake job"
        raw <- mkImmutableGitInputFlakeThunk evalState (ViaJSON.fromViaJSON srcInput)
        let pso :: PSObject (Schema.Attrs '[])
            pso = Schema.PSObject {value = raw, provenance = Schema.Other "flake.nix"}
        toValue evalState pso
    else escalateAs UserException =<< liftIO (loadNixFile evalState (toS $ Eval.cwd eval) (coerce $ Eval.gitSource eval))

walkOnPush :: (MonadEval m, MonadUnliftIO m, KatipContext m) => EvalEnv -> OnPush.OnPush -> PSObject HerculesCISchema -> ConduitT i Event m ()
walkOnPush evalEnv onPushParams herculesCI = do
  handler <- herculesCI #?! #onPush >>= requireDict (OnPush.name onPushParams)
  walkHandler evalEnv (OnPush.inputs onPushParams) handler

walkOnSchedule :: (MonadEval m, MonadUnliftIO m, KatipContext m) => EvalEnv -> OnSchedule.OnSchedule -> PSObject HerculesCISchema -> ConduitT i Event m ()
walkOnSchedule evalEnv onScheduleParams herculesCI = do
  handler <- herculesCI #?! #onSchedule >>= requireDict (OnSchedule.name onScheduleParams)
  walkHandler evalEnv (OnSchedule.extraInputs onScheduleParams) handler

walkHandler ::
  ( MonadEval m,
    MonadUnliftIO m,
    KatipContext m,
    handlerAttrs . "outputs" ~ (InputsSchema ->? OutputsSchema)
  ) =>
  EvalEnv ->
  Map Text API.ImmutableInput.ImmutableInput ->
  PSObject (Schema.Attrs' handlerAttrs w) ->
  ConduitT i Event m ()
walkHandler evalEnv inputs handler = do
  let evalState = evalEnvState evalEnv
  inputs' <- liftIO $ do
    inputs' <- for inputs \input -> do
      inputToValue evalState input
    toRawValue evalState inputs'
  outputsFun <- handler #. #outputs
  outputs <- outputsFun $? (Schema.PSObject {value = inputs', provenance = Schema.Data})
  simpleWalk evalEnv (Schema.value outputs)

inputToValue :: Ptr EvalState -> API.ImmutableInput.ImmutableInput -> IO RawValue
inputToValue evalState (API.ImmutableInput.ArchiveUrl u) = getFlakeFromArchiveUrl evalState u
inputToValue evalState (API.ImmutableInput.Git g) = mkImmutableGitInputFlakeThunk evalState g

mkImmutableGitInputFlakeThunk :: Ptr EvalState -> API.ImmutableGitInput.ImmutableGitInput -> IO RawValue
mkImmutableGitInputFlakeThunk evalState git = do
  -- TODO: allow picking ssh/http url
  getFlakeFromGit
    evalState
    (API.ImmutableGitInput.httpURL git)
    (API.ImmutableGitInput.ref git)
    (API.ImmutableGitInput.rev git)

sendConfig :: (MonadIO m) => Ptr EvalState -> Bool -> PSObject HerculesCISchema -> ConduitT i Event m ()
sendConfig evalState isFlake herculesCI = flip runReaderT evalState $ do
  herculesCI #? #onPush >>= traverse_ \onPushes -> do
    attrs <- dictionaryToMap onPushes
    for_ (M.mapWithKey (,) attrs) \(name, onPush) -> do
      ei <- onPush #? #extraInputs >>= traverse parseExtraInputs
      enable <- onPush #? #enable >>= traverse fromPSObject <&> fromMaybe True
      when enable . lift . yield . Event.OnPushHandler . ViaJSON.ViaJSON $
        OnPushHandlerEvent
          { handlerName = decodeUtf8 name,
            handlerExtraInputs = M.mapKeys decodeUtf8 (fromMaybe mempty ei),
            isFlake = isFlake
          }
  herculesCI #? #onSchedule >>= traverse_ \onSchedules -> do
    attrs <- dictionaryToMap onSchedules
    for_ (M.mapWithKey (,) attrs) \(name, onSchedule) -> do
      ei <- onSchedule #? #extraInputs >>= traverse parseExtraInputs
      enable <- onSchedule #? #enable >>= traverse fromPSObject <&> fromMaybe True
      when_ <- onSchedule #?? #when >>= maybe (pure defaultConstraints) parseWhen
      when enable . lift . yield . Event.OnScheduleHandler . ViaJSON.ViaJSON $
        OnScheduleHandlerEvent
          { handlerName = decodeUtf8 name,
            handlerExtraInputs = M.mapKeys decodeUtf8 (fromMaybe mempty ei),
            isFlake = isFlake,
            when = when_
          }

defaultConstraints :: TimeConstraints
defaultConstraints = noConstraints

noConstraints :: TimeConstraints
noConstraints = TimeConstraints Nothing Nothing Nothing Nothing

parseWhen :: (MonadEval m) => PSObject NixFile.TimeConstraintsSchema -> m Hercules.API.Agent.Evaluate.EvaluateEvent.OnScheduleHandlerEvent.TimeConstraints
parseWhen w = do
  minute_ <-
    w #?? #minute >>= traverse \obj -> do
      v <- fromPSObject obj
      if v >= 0 && v < 60
        then pure v
        else throwIO $ Schema.InvalidValue (provenance obj) $ "minute value " <> show v <> " is out of range [0..59]."

  let validateHour obj = do
        v <- fromPSObject obj
        if v >= 0 && v < 24
          then pure v
          else throwIO $ Schema.InvalidValue (provenance obj) $ "hour value " <> show v <> " is out of range [0..23]."

  hour_ <-
    w
      #?? #hour
      >>= traverse
        ( (\oneInt -> validateHour oneInt <&> \x -> [x])
            |! ( \hours -> do
                   v <- traverseArray validateHour hours
                   when (null v) do
                     throwIO $ Schema.InvalidValue (provenance hours) $ "hour must not be an empty list."
                   pure v
               )
        )
  dayOfWeek <-
    w
      #?? #dayOfWeek
      >>= traverse
        ( \dayStringsObject -> do
            days <- traverseArray parseDayOfWeek dayStringsObject
            when (null days) do
              throwIO $ Schema.InvalidValue (provenance dayStringsObject) $ "dayOfWeek must not be an empty list."
            pure days
        )
  dayOfMonth <-
    w
      #?? #dayOfMonth
      >>= traverse \daysOfMonth -> do
        v <-
          daysOfMonth & traverseArray \obj -> do
            v <- fromPSObject obj
            if v >= 0 && v <= 31
              then pure v
              else throwIO $ Schema.InvalidValue (provenance obj) $ "month value " <> show v <> " is out of range [0..31]."
        when (null v) do
          throwIO $ Schema.InvalidValue (provenance daysOfMonth) $ "dayOfMonth must not be an empty list."
        pure v

  pure $
    TimeConstraints
      { minute = (minute_ :: Maybe Int64) <&> fromIntegral,
        hour = (hour_ :: Maybe [Int64]) <&> fmap fromIntegral,
        dayOfWeek = dayOfWeek,
        dayOfMonth = (dayOfMonth :: Maybe [Int64]) <&> fmap fromIntegral
      }

parseDayOfWeek :: (Schema.FromPSObject a Text, MonadEval m) => PSObject a -> m DayOfWeek
parseDayOfWeek dayString = do
  s <- fromPSObject @_ @Text dayString
  case T.toLower s of
    "mon" -> pure Mon
    "tue" -> pure Tue
    "wed" -> pure Wed
    "thu" -> pure Thu
    "fri" -> pure Fri
    "sat" -> pure Sat
    "sun" -> pure Sun
    other ->
      throwIO
        ( Schema.InvalidValue
            (provenance dayString)
            ("Expected an abbreviated day of week: \"Mon\", \"Tue\", \"Wed\", \"Thu\", \"Fri\", \"Sat\" or \"Sun\". Actual: " <> show other)
        )

data TreeWork = TreeWork
  { treeWorkAttrPath :: [ByteString],
    -- Redundant but convenient
    treeWorkThunk :: RawValue,
    -- Redundant but convenient
    treeWorkRemainingDepth :: Int
  }

type Enqueue i m o = i -> (o -> m ()) -> IO ()

pollingWith ::
  forall i m o.
  (Ord i, MonadIO m) =>
  -- | A 'finally'-like function that powers an assertion.
  --
  -- Not used for resource allocation; does not have to be perfect.
  (m () -> IO () -> m ()) ->
  -- | Arg will not be empty. Return must not be empty.
  (forall x. Map i x -> m (Map i o)) ->
  -- | Do work. Enqueue function can be called from anywhere, but will be ignored after the returned action is done.
  ((i -> (o -> m ()) -> IO ()) -> m ()) ->
  -- | Iterate until work done.
  m ()
pollingWith ffinally poller startWork = do
  q <- liftIO (newIORef mempty)
  operational <- liftIO (newIORef True)

  let checkOperational = do
        op <- readIORef operational
        when (not op) (panic "pollingWith has stopped. Can not continue.")

      enqueue :: Enqueue i m o
      enqueue i f = do
        checkOperational
        modifyIORef' q $
          M.alter
            \case
              Nothing -> Just f
              Just f0 -> Just (\o -> f0 o *> f o)
            i

      loop :: m ()
      loop = do
        -- peek all, restore blocked next
        q0 <- liftIO (atomicModifyIORef' q (mempty,))
        if null q0
          then pass
          else do
            justCompleted <- poller q0
            let blocked = M.difference q0 justCompleted

            when (null justCompleted) do
              panic "pollingWith: poller must not return empty"

            liftIO (atomicModifyIORef' q \q1 -> (M.unionWith (liftA2 (>>)) blocked q1, ()))

            sequenceA_ $ M.intersectionWith ($) q0 justCompleted

            loop

  startWork enqueue
  loop `ffinally` writeIORef operational False

handleExceptions ::
  (Exception t, MonadUnliftIO m) =>
  Store ->
  ( [ByteString] ->
    t ->
    (Either SomeException b -> ConduitT i Event m ()) ->
    ConduitT i Event m ()
  ) ->
  [ByteString] ->
  ConduitT i Event m () ->
  ConduitT i Event m ()
handleExceptions store enqueue path m = go
  where
    go =
      -- not catchC or handleC, to make sure async exceptions aren't masked
      tryC m >>= \case
        Left e -> handler e
        Right r -> pure r

    handler e | isAsyncException e = throwIO e
    handler e = case fromException e of
      Just rr -> do
        enqueue path rr \case
          Right {} -> go
          Left exception ->
            -- TODO: throw a Nix native exception in the builder callback instead?
            --       trace would have to be preserved from the first execution
            yieldAttributeError store path exception
      Nothing -> do
        yieldAttributeError store path e

-- | Documented in @docs/modules/ROOT/pages/evaluation.adoc@.
simpleWalk ::
  (MonadUnliftIO m, KatipContext m) =>
  EvalEnv ->
  RawValue ->
  ConduitT i Event m ()
simpleWalk evalEnv initialThunk = do
  let store = evalEnvStore evalEnv
      evalState = evalEnvState evalEnv

      walk' ::
        (MonadUnliftIO m, KatipContext m) =>
        ([ByteString] -> AsyncRealisationRequired -> (Either SomeException () -> ConduitT i1 Event m ()) -> ConduitT i1 Event m ()) ->
        TreeWork ->
        -- Program that performs the walk and emits 'Event's
        ConduitT i1 Event m ()
      walk' enqueue treeWork = do
        let path = treeWorkAttrPath treeWork
            depthRemaining = treeWorkRemainingDepth treeWork
            v = treeWorkThunk treeWork

        handleExceptions store enqueue path do
          m <- liftIO $ escalate =<< match evalState v
          case m of
            IsAttrs attrValue -> do
              isDeriv <- liftIO $ isDerivation evalState v
              if isDeriv
                then walkDerivation store evalState False path attrValue
                else do
                  attrs <- liftIO $ getAttrs evalState attrValue
                  recurse <- shouldWalkIntoAttrsAndLog path evalState attrs
                  when recurse . void $
                    flip M.traverseWithKey attrs $
                      \name value ->
                        if depthRemaining > 0
                          then
                            walk'
                              enqueue
                              TreeWork
                                { treeWorkRemainingDepth = depthRemaining - 1,
                                  treeWorkAttrPath = path ++ [name],
                                  treeWorkThunk = value
                                }
                          else yield (Event.Error $ "Max recursion depth reached at path " <> show (path ++ [name]))
            _any -> do
              vt <- liftIO $ rawValueType v
              unless
                ( lastMay path == Just "recurseForDerivations"
                    && vt == Hercules.CNix.Expr.Raw.Bool
                )
                do
                  logLocM DebugS $ logStr $ "Ignoring " <> show path <> " : " <> (show vt :: Text)

  withIFDQueue evalEnv \enqueue ->
    walk'
      enqueue
      TreeWork
        { treeWorkAttrPath = [],
          treeWorkRemainingDepth = 10,
          treeWorkThunk = initialThunk
        }

shouldWalkIntoAttrsAndLog :: (KatipContext m) => [ByteString] -> Ptr EvalState -> Map ByteString RawValue -> m Bool
shouldWalkIntoAttrsAndLog path evalState attrs =
  case M.lookup "_type" attrs of
    Just v -> do
      -- `_type` value should be cheap, so let's log it
      liftIO (match evalState v) >>= \case
        Right (IsString sv) -> do
          s <- liftIO $ getStringIgnoreContext sv
          logLocM DebugS $ logStr ("Ignoring " <> show path <> " : _type = " <> show s :: Text)
        Right _ -> do
          logLocM DebugS $ logStr ("Ignoring " <> show path <> " : _type is not a string" :: Text)
        Left _ -> do
          logLocM DebugS $ logStr ("Ignoring " <> show path <> " : _type contains error" :: Text)
      pure False
    Nothing -> do
      pure True

withIFDQueue ::
  (MonadUnliftIO m) =>
  EvalEnv ->
  ( ([ByteString] -> AsyncRealisationRequired -> (Either SomeException () -> ConduitT i Event m ()) -> ConduitT i Event m ()) ->
    ConduitT i Event m ()
  ) ->
  ConduitT i Event m ()
withIFDQueue evalEnv doIt = do
  let poller ::
        (MonadIO m) =>
        Map (StorePath, ByteString) x ->
        ConduitT i Event m (Map (StorePath, ByteString) (Either SomeException ()))
      poller q = do
        liftIO $ atomically do
          allBuilds <- readTVar (drvOutputSubstituteAsyncs (evalEnvHerculesState evalEnv))
          x <-
            q & M.traverseWithKey \qKey _ -> do
              case M.lookup qKey allBuilds of
                Nothing -> panic "A realisation should have been started before throwing AsyncRealisationRequired"
                Just asy -> pollSTM asy
          let completed = void <$> M.mapMaybe identity x
          guard (not (null completed))
          pure completed

      bestEffortFinally f ff = catchC f (\e -> liftIO ff >> throwIO (e :: SomeException))

      betterEnqueue enqueue attrPath rr m = do
        drvPath <- liftIO $ storePathToPath (evalEnvStore evalEnv) $ realisationRequiredDerivationPath rr
        let ev =
              Event.AttributeIFD.AttributeIFD
                { path = attrPath,
                  derivationPath = drvPath,
                  derivationOutput = realisationRequiredOutputName rr,
                  done = False
                }
        yield (Event.AttributeIFD ev)
        liftIO $ enqueue (realisationRequiredDerivationPath rr, realisationRequiredOutputName rr) \outcome -> do
          yield (Event.AttributeIFD $ ev {Event.AttributeIFD.done = True})
          m outcome

  reset <- liftIO $ readIORef (evalEnvIsNonBlocking evalEnv)
  liftIO $ writeIORef (evalEnvIsNonBlocking evalEnv) True

  pollingWith bestEffortFinally poller (doIt . betterEnqueue)

  liftIO $ writeIORef (evalEnvIsNonBlocking evalEnv) reset

traverseSPWOs :: (StorePathWithOutputs -> IO ()) -> StdVector NixStorePathWithOutputs -> IO ()
traverseSPWOs f v = do
  v' <- Std.Vector.toListFP v
  traverse_ f v'

-- | Documented in @docs/modules/ROOT/pages/legacy-evaluation.adoc@.
walk ::
  forall i m.
  (MonadUnliftIO m, KatipContext m) =>
  EvalEnv ->
  Value NixAttrs ->
  RawValue ->
  ConduitT i Event m ()
walk evalEnv autoArgs v0 = withIFDQueue evalEnv \enqueue ->
  let start = walk' True [] 10 v0
      store = evalEnvStore evalEnv
      evalState = evalEnvState evalEnv
      walk' ::
        -- If True, always walk this attribute set. Only True for the root.
        Bool ->
        -- Attribute path
        [ByteString] ->
        -- Depth of tree remaining
        Integer ->
        -- Current node of the walk
        RawValue ->
        -- Program that performs the walk and emits 'Event's
        ConduitT i Event m ()
      walk' forceWalkAttrset path depthRemaining v =
        -- logLocM DebugS $ logStr $ "Walking " <> (show path :: Text)
        handleExceptions store enqueue path $
          liftIO (match evalState v)
            >>= \case
              Left e ->
                throwIO e
              Right m -> case m of
                IsAttrs attrValue -> do
                  isDeriv <- liftIO $ isDerivation evalState v
                  if isDeriv
                    then walkDerivation store evalState True path attrValue
                    else do
                      walkAttrset <-
                        if forceWalkAttrset
                          then pure True
                          else -- Hydra doesn't seem to obey this, because it walks the
                          -- x64_64-linux etc attributes per package. Maybe those
                          -- are special cases?
                          -- For now, we will assume that people don't build a whole Nixpkgs
                            liftIO $ getRecurseForDerivations evalState attrValue
                      isfunctor <- liftIO $ isFunctor evalState v
                      if isfunctor && walkAttrset
                        then do
                          x <- liftIO (autoCallFunction evalState v autoArgs)
                          walk' True path (depthRemaining - 1) x
                        else do
                          attrs <- liftIO $ getAttrs evalState attrValue
                          void $
                            flip M.traverseWithKey attrs $
                              \name value ->
                                when (depthRemaining > 0 && walkAttrset) $
                                  walk' -- TODO: else warn
                                    False
                                    (path ++ [name])
                                    (depthRemaining - 1)
                                    value
                _any -> do
                  vt <- liftIO $ rawValueType v
                  unless
                    ( lastMay path
                        == Just "recurseForDerivations"
                        && vt
                          == Hercules.CNix.Expr.Raw.Bool
                    )
                    $ logLocM DebugS
                    $ logStr
                    $ "Ignoring "
                      <> show path
                      <> " : "
                      <> (show vt :: Text)
                  pass
   in start

-- | Documented in @docs/modules/ROOT/pages/evaluation.adoc@.
walkDerivation ::
  (MonadIO m) =>
  Store ->
  Ptr EvalState ->
  Bool ->
  [ByteString] ->
  Value NixAttrs ->
  ConduitT i Event m ()
walkDerivation store evalState effectsAnywhere path attrValue = do
  drvStorePath <- getDrvFile evalState (rtValue attrValue)
  drvPath <- liftIO $ CNix.storePathToPath store drvStorePath
  typE <- runExceptT do
    isEffect <- liftEitherAs Left =<< liftIO (getAttrBool evalState attrValue "isEffect")
    case isEffect of
      Just True
        | effectsAnywhere
            || inEffects path ->
            throwE $ Right Attribute.Effect
      Just True | otherwise -> throwE $ Left $ toException $ UserException "This derivation is marked as an effect, but effects are only allowed below the effects attribute."
      _ -> pass
    isDependenciesOnly <- liftEitherAs Left =<< liftIO (getAttrBool evalState attrValue "buildDependenciesOnly")
    case isDependenciesOnly of
      Just True -> throwE $ Right Attribute.DependenciesOnly
      _ -> pass
    phases <- liftEitherAs Left =<< liftIO (getAttrList evalState attrValue "phases")
    case phases of
      Just [aSingularPhase] ->
        liftIO (match evalState aSingularPhase) >>= liftEitherAs Left >>= \case
          IsString phaseNameValue -> do
            phaseName <- liftIO (getStringIgnoreContext phaseNameValue)
            when (phaseName == "nobuildPhase") do
              throwE $ Right Attribute.DependenciesOnly
          _ -> pass
      _ -> pass
    mayFail <- liftEitherAs Left =<< liftIO (getAttrBool evalState attrValue "ignoreFailure")
    case mayFail of
      Just True -> throwE $ Right Attribute.MayFail
      _ -> pass
    mustFail <- liftEitherAs Left =<< liftIO (getAttrBool evalState attrValue "requireFailure")
    case mustFail of
      Just True -> throwE $ Right Attribute.MustFail
      _ -> pass
  let yieldAttribute typ =
        yield $
          Event.Attribute
            Attribute.Attribute
              { Attribute.path = path,
                Attribute.drv = drvPath,
                Attribute.typ = typ
              }
  case typE of
    Left (Left e) -> yieldAttributeError store path e
    Left (Right t) -> yieldAttribute t
    Right _ -> yieldAttribute Attribute.Regular
  where
    inEffects :: [ByteString] -> Bool
    inEffects ("effects" : _) = True
    inEffects _ = False

liftEitherAs :: (MonadError e m) => (e0 -> e) -> Either e0 a -> m a
liftEitherAs f = liftEither . rmap
  where
    rmap (Left e) = Left (f e)
    rmap (Right a) = Right a
