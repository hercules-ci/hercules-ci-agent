{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Agent.Worker.Evaluate where

import Conduit
import Control.Concurrent.STM hiding (check)
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Data.Coerce (coerce)
import qualified Data.Conduit
import Data.Conduit.Katip.Orphans ()
import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import Hercules.API.Agent.Evaluate.EvaluateEvent.OnPushHandlerEvent (OnPushHandlerEvent (OnPushHandlerEvent))
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.OnPushHandlerEvent
import qualified Hercules.API.Agent.Evaluate.EvaluateTask as EvaluateTask
import qualified Hercules.API.Agent.Evaluate.EvaluateTask.OnPush as OnPush
import qualified Hercules.API.Agent.Evaluate.ImmutableGitInput as API.ImmutableGitInput
import qualified Hercules.API.Agent.Evaluate.ImmutableInput as API.ImmutableInput
import Hercules.Agent.NixFile (HerculesCISchema, getHerculesCI, homeExprRawValue, loadNixFile, parseExtraInputs)
import qualified Hercules.Agent.NixFile as NixFile
import Hercules.Agent.NixFile.HerculesCIArgs (CISystems (CISystems), HerculesCIMeta (HerculesCIMeta), fromGitSource)
import qualified Hercules.Agent.NixFile.HerculesCIArgs
import Hercules.Agent.Worker.Env (HerculesState (..))
import Hercules.Agent.Worker.Error (renderException)
import Hercules.Agent.Worker.HerculesStore (nixStore, setBuilderCallback)
import qualified Hercules.Agent.WorkerProtocol.Command.BuildResult as BuildResult
import Hercules.Agent.WorkerProtocol.Command.Eval
  ( Eval,
  )
import qualified Hercules.Agent.WorkerProtocol.Command.Eval as Eval
import Hercules.Agent.WorkerProtocol.Event
  ( Event,
    ViaJSON (ViaJSON),
  )
import qualified Hercules.Agent.WorkerProtocol.Event as Event
import qualified Hercules.Agent.WorkerProtocol.Event.Attribute as Attribute
import qualified Hercules.Agent.WorkerProtocol.Event.AttributeError as AttributeError
import Hercules.CNix as CNix
import Hercules.CNix.Expr (Match (IsAttrs, IsString), NixAttrs, RawValue, addAllowedPath, addInternalAllowedPaths, autoCallFunction, evalArgs, getAttrBool, getAttrList, getAttrs, getDrvFile, getFlakeFromArchiveUrl, getFlakeFromGit, getRecurseForDerivations, getStringIgnoreContext, isDerivation, isFunctor, match, rawValueType, rtValue, toRawValue, toValue, withEvalStateConduit)
import Hercules.CNix.Expr.Context (EvalState)
import qualified Hercules.CNix.Expr.Raw
import Hercules.CNix.Expr.Schema (MonadEval, PSObject, dictionaryToMap, fromPSObject, requireDict, (#.), (#?), (#?!), ($?))
import qualified Hercules.CNix.Expr.Schema as Schema
import Hercules.CNix.Expr.Typed (Value)
import Hercules.CNix.Std.Vector (StdVector)
import qualified Hercules.CNix.Std.Vector as Std.Vector
import Hercules.CNix.Store.Context (NixStorePathWithOutputs)
import Hercules.Error
import Hercules.UserException (UserException (UserException))
import Katip
import Protolude hiding (bracket, catch, check, evalState, wait, withAsync, yield)
import qualified UnliftIO
import UnliftIO.Exception (bracket, catch)
import Prelude ()

data BuildException = BuildException
  { buildExceptionDerivationPath :: Text,
    buildExceptionDetail :: Maybe Text
  }
  deriving (Show, Typeable)

instance Exception BuildException

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
withDrvInProgress :: MonadUnliftIO m => HerculesState -> StorePath -> m a -> m a
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

yieldAttributeError :: MonadIO m => [ByteString] -> SomeException -> ConduitT i Event m ()
yieldAttributeError path e
  | (Just e') <- fromException e =
    yield $
      Event.AttributeError $
        AttributeError.AttributeError
          { AttributeError.path = path,
            AttributeError.message =
              "Could not build derivation " <> buildExceptionDerivationPath e'
                <> ", which is required during evaluation."
                <> foldMap (" " <>) (buildExceptionDetail e'),
            AttributeError.errorDerivation = Just (buildExceptionDerivationPath e'),
            AttributeError.errorType = Just "BuildException",
            AttributeError.trace = Nothing -- would be nice to get a trace here. Throw and catch more C++ exception types?
          }
yieldAttributeError path e = do
  (e', maybeTrace) <- liftIO $ renderException e
  yield $
    Event.AttributeError $
      AttributeError.AttributeError
        { AttributeError.path = path,
          AttributeError.message = e',
          AttributeError.errorDerivation = Nothing,
          AttributeError.errorType = Just (show (typeOf e)),
          AttributeError.trace = maybeTrace
        }

maybeThrowBuildException :: MonadIO m => BuildResult.BuildStatus -> Text -> m ()
maybeThrowBuildException result plainDrvText =
  case result of
    BuildResult.Failure -> throwIO $ BuildException plainDrvText Nothing
    BuildResult.DependencyFailure -> throwIO $ BuildException plainDrvText (Just "A dependency could not be built.")
    BuildResult.Success -> pass

mkCache :: forall k a m. (MonadUnliftIO m, Ord k) => IO (k -> m a -> m a)
mkCache = do
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
              modifyIORef doneRef (M.insert p (r :: Either SomeException a))
              escalate r
  pure cacheBy

runEval ::
  forall i m.
  (MonadResource m, KatipContext m, MonadUnliftIO m, MonadThrow m) =>
  HerculesState ->
  Eval ->
  ConduitM i Event m ()
runEval st@HerculesState {herculesStore = hStore, shortcutChannel = shortcutChan, drvsCompleted = drvsCompl} eval = do
  for_ (Eval.extraNixOptions eval) $ liftIO . uncurry setGlobalOption
  for_ (Eval.extraNixOptions eval) $ liftIO . uncurry setOption
  let store = nixStore hStore
      isFlake = Eval.isFlakeJob eval
  s <- storeUri store
  UnliftIO unlift <- lift askUnliftIO
  let decode = decodeUtf8With lenientDecode

  cachingBuilt <- liftIO mkCache

  liftIO . setBuilderCallback hStore $
    traverseSPWOs $ \storePathWithOutputs -> unlift $ do
      drvStorePath <- liftIO $ getStorePath storePathWithOutputs
      drvPath <- liftIO $ CNix.storePathToPath store drvStorePath
      cachingBuilt drvPath do
        let pathText = decode drvPath
        outputs <- liftIO $ getOutputs storePathWithOutputs
        katipAddContext (sl "fullpath" pathText) $
          for_ outputs $ \outputName -> do
            withDrvInProgress st drvStorePath $ do
              derivation <- liftIO $ getDerivation store drvStorePath
              drvName <- liftIO $ getDerivationNameFromPath drvStorePath
              drvOutputs <- liftIO $ getDerivationOutputs store drvName derivation
              outputPath <-
                case find (\o -> derivationOutputName o == outputName) drvOutputs of
                  Nothing -> panic $ "output " <> show outputName <> " does not exist on " <> pathText
                  Just o -> case derivationOutputPath o of
                    Just x -> pure x
                    Nothing ->
                      -- FIXME ca-derivations
                      panic $ "output path unknown for output " <> show outputName <> " on " <> pathText <> ". ca-derivations is not supported yet."

              isValid <- liftIO $ isValidPath store outputPath
              if isValid
                then do
                  logLocM DebugS "Output already valid"
                  -- Report IFD
                  liftIO $ writeChan shortcutChan $ Just $ Event.Build drvPath (decode outputName) Nothing False
                else do
                  logLocM DebugS "Building"
                  liftIO $ writeChan shortcutChan $ Just $ Event.Build drvPath (decode outputName) Nothing True
                  ( katipAddContext (sl "outputPath" (show outputPath :: Text)) do
                      logLocM DebugS "Attempting early ensurePath"
                      liftIO (ensurePath (wrappedStore st) outputPath)
                    )
                    `catch` \e0 -> do
                      katipAddContext (sl "message" (show (e0 :: SomeException) :: Text)) $
                        logLocM DebugS "Recovering from failed wrapped.ensurePath"
                      (attempt0, result) <-
                        liftIO $
                          atomically $ do
                            c <- readTVar drvsCompl
                            anyAlternative $ M.lookup drvStorePath c
                      liftIO $ maybeThrowBuildException result (decode drvPath)
                      liftIO clearSubstituterCaches
                      liftIO $ clearPathInfoCache store
                      liftIO (ensurePath (wrappedStore st) outputPath) `catch` \e1 -> do
                        katipAddContext (sl "message" (show (e1 :: SomeException) :: Text)) $
                          logLocM DebugS "Recovering from fresh ensurePath"
                        liftIO $ writeChan shortcutChan $ Just $ Event.Build drvPath (decode outputName) (Just attempt0) True
                        -- TODO sync
                        result' <-
                          liftIO $
                            atomically $ do
                              c <- readTVar drvsCompl
                              (attempt1, r) <- anyAlternative $ M.lookup drvStorePath c
                              guard (attempt1 /= attempt0)
                              pure r
                        liftIO $ maybeThrowBuildException result' (decode drvPath)
                        liftIO clearSubstituterCaches
                        liftIO $ clearPathInfoCache store
                        liftIO (ensurePath (wrappedStore st) outputPath) `catch` \e2 ->
                          liftIO $
                            throwIO $
                              BuildException
                                (decode drvPath)
                                ( Just $
                                    "It could not be retrieved on the evaluating agent, despite a successful rebuild. Exception: "
                                      <> show (e2 :: SomeException)
                                )
              liftIO $ addTemporaryRoot store outputPath
            logLocM DebugS "Built"
  withEvalStateConduit store $ \evalState -> do
    liftIO do
      addInternalAllowedPaths evalState
      for_ (Eval.allowedPaths eval) (addAllowedPath evalState)
    katipAddContext (sl "storeURI" (decode s)) $
      logLocM DebugS "EvalState loaded."
    args <-
      liftIO $
        evalArgs evalState (autoArgArgs (Eval.autoArguments eval))
    Data.Conduit.handleC (yieldAttributeError []) $
      do
        homeExpr <- getHomeExpr evalState eval
        let hargs = fromGitSource (coerce $ Eval.gitSource eval) meta
            meta = HerculesCIMeta {apiBaseUrl = Eval.apiBaseUrl eval, ciSystems = CISystems (Eval.ciSystems eval)}
        liftIO (flip runReaderT evalState $ getHerculesCI homeExpr hargs) >>= \case
          Nothing ->
            -- legacy
            walk store evalState args (homeExprRawValue homeExpr)
          Just herculesCI -> do
            case Event.fromViaJSON (Eval.selector eval) of
              EvaluateTask.ConfigOrLegacy -> do
                yield Event.JobConfig
                sendConfig evalState isFlake herculesCI
              EvaluateTask.OnPush onPush ->
                transPipe (`runReaderT` evalState) do
                  walkOnPush store evalState onPush herculesCI
    yield Event.EvaluationDone

getHomeExpr :: (MonadThrow m, MonadIO m) => Ptr EvalState -> Eval -> m NixFile.HomeExpr
getHomeExpr evalState eval =
  if Eval.isFlakeJob eval
    then
      NixFile.Flake <$> liftIO do
        srcInput <- case Eval.srcInput eval of
          Just x -> pure x
          Nothing -> panic "srcInput is required for flake job"
        raw <- mkImmutableGitInputFlakeThunk evalState (Event.fromViaJSON srcInput)
        let pso :: PSObject (Schema.Attrs '[])
            pso = Schema.PSObject {value = raw, provenance = Schema.Other "flake.nix"}
        toValue evalState pso
    else escalateAs UserException =<< liftIO (loadNixFile evalState (toS $ Eval.cwd eval) (coerce $ Eval.gitSource eval))

walkOnPush :: (MonadEval m, MonadUnliftIO m, KatipContext m, MonadThrow m) => Store -> Ptr EvalState -> OnPush.OnPush -> PSObject HerculesCISchema -> ConduitT i Event m ()
walkOnPush store evalState onPushParams herculesCI = do
  onPushHandler <- herculesCI #?! #onPush >>= requireDict (OnPush.name onPushParams)
  inputs <- liftIO $ do
    inputs <- for (OnPush.inputs onPushParams) \input -> do
      inputToValue evalState input
    toRawValue evalState inputs
  outputsFun <- onPushHandler #. #outputs
  outputs <- outputsFun $? (Schema.PSObject {value = inputs, provenance = Schema.Data})
  simpleWalk store evalState (Schema.value outputs)

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

sendConfig :: MonadIO m => Ptr EvalState -> Bool -> PSObject HerculesCISchema -> ConduitT i Event m ()
sendConfig evalState isFlake herculesCI = flip runReaderT evalState $ do
  herculesCI #? #onPush >>= traverse_ \onPushes -> do
    attrs <- dictionaryToMap onPushes
    for_ (M.mapWithKey (,) attrs) \(name, onPush) -> do
      ei <- onPush #? #extraInputs >>= traverse parseExtraInputs
      enable <- onPush #? #enable >>= traverse fromPSObject <&> fromMaybe True
      when enable . lift . yield . Event.OnPushHandler . ViaJSON $
        OnPushHandlerEvent
          { handlerName = decodeUtf8 name,
            handlerExtraInputs = M.mapKeys decodeUtf8 (fromMaybe mempty ei),
            isFlake = isFlake
          }

-- | Documented in @docs/modules/ROOT/pages/evaluation.adoc@.
simpleWalk ::
  (MonadUnliftIO m, KatipContext m) =>
  Store ->
  Ptr EvalState ->
  RawValue ->
  ConduitT i Event m ()
simpleWalk store evalState = walk' [] 10
  where
    handleErrors path = Data.Conduit.handleC (yieldAttributeError path)
    walk' ::
      (MonadUnliftIO m, KatipContext m) =>
      -- Attribute path
      [ByteString] ->
      -- Depth of tree remaining
      Integer ->
      -- Current node of the walk
      RawValue ->
      -- Program that performs the walk and emits 'Event's
      ConduitT i1 Event m ()
    walk' path depthRemaining v =
      handleErrors path $
        liftIO (match evalState v)
          >>= \case
            Left e ->
              yieldAttributeError path e
            Right m -> case m of
              IsAttrs attrValue -> do
                isDeriv <- liftIO $ isDerivation evalState v
                if isDeriv
                  then walkDerivation store evalState False path attrValue
                  else do
                    attrs <- liftIO $ getAttrs attrValue
                    void $
                      flip M.traverseWithKey attrs $
                        \name value ->
                          if depthRemaining > 0
                            then
                              walk'
                                (path ++ [name])
                                (depthRemaining - 1)
                                value
                            else yield (Event.Error $ "Max recursion depth reached at path " <> show path)
              _any -> do
                vt <- liftIO $ rawValueType v
                unless
                  ( lastMay path == Just "recurseForDerivations"
                      && vt == Hercules.CNix.Expr.Raw.Bool
                  )
                  do
                    logLocM DebugS $
                      logStr $
                        "Ignoring "
                          <> show path
                          <> " : "
                          <> (show vt :: Text)

traverseSPWOs :: (StorePathWithOutputs -> IO ()) -> StdVector NixStorePathWithOutputs -> IO ()
traverseSPWOs f v = do
  v' <- Std.Vector.toListFP v
  traverse_ f v'

-- | Documented in @docs/modules/ROOT/pages/legacy-evaluation.adoc@.
walk ::
  (MonadUnliftIO m, KatipContext m) =>
  Store ->
  Ptr EvalState ->
  Value NixAttrs ->
  RawValue ->
  ConduitT i Event m ()
walk store evalState = walk' True [] 10
  where
    handleErrors path = Data.Conduit.handleC (yieldAttributeError path)
    walk' ::
      (MonadUnliftIO m, KatipContext m) =>
      -- If True, always walk this attribute set. Only True for the root.
      Bool ->
      -- Attribute path
      [ByteString] ->
      -- Depth of tree remaining
      Integer ->
      -- Auto arguments to pass to (attrset-)functions
      Value NixAttrs ->
      -- Current node of the walk
      RawValue ->
      -- Program that performs the walk and emits 'Event's
      ConduitT i1 Event m ()
    walk' forceWalkAttrset path depthRemaining autoArgs v =
      -- logLocM DebugS $ logStr $ "Walking " <> (show path :: Text)
      handleErrors path $
        liftIO (match evalState v)
          >>= \case
            Left e ->
              yieldAttributeError path e
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
                        walk' True path (depthRemaining - 1) autoArgs x
                      else do
                        attrs <- liftIO $ getAttrs attrValue
                        void $
                          flip M.traverseWithKey attrs $
                            \name value ->
                              when (depthRemaining > 0 && walkAttrset) $
                                walk' -- TODO: else warn
                                  False
                                  (path ++ [name])
                                  (depthRemaining - 1)
                                  autoArgs
                                  value
              _any -> do
                vt <- liftIO $ rawValueType v
                unless
                  ( lastMay path
                      == Just "recurseForDerivations"
                      && vt
                      == Hercules.CNix.Expr.Raw.Bool
                  )
                  $ logLocM DebugS $
                    logStr $
                      "Ignoring "
                        <> show path
                        <> " : "
                        <> (show vt :: Text)
                pass

-- | Documented in @docs/modules/ROOT/pages/evaluation.adoc@.
walkDerivation ::
  MonadIO m =>
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
    Left (Left e) -> yieldAttributeError path e
    Left (Right t) -> yieldAttribute t
    Right _ -> yieldAttribute Attribute.Regular
  where
    inEffects :: [ByteString] -> Bool
    inEffects ("effects" : _) = True
    inEffects _ = False

liftEitherAs :: MonadError e m => (e0 -> e) -> Either e0 a -> m a
liftEitherAs f = liftEither . rmap
  where
    rmap (Left e) = Left (f e)
    rmap (Right a) = Right a
