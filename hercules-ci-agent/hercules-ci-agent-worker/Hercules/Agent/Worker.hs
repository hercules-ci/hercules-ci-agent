{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

module Hercules.Agent.Worker
  ( main,
  )
where

import Conduit
import Control.Concurrent.STM
import qualified Control.Exception.Lifted as EL
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control
import qualified Data.Conduit
import Data.Conduit.Extras (sinkChan, sinkChanTerminate, sourceChan)
import Data.Conduit.Katip.Orphans ()
import Data.Conduit.Serialization.Binary
  ( conduitDecode,
    conduitEncode,
  )
import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import Data.UUID (UUID)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Hercules.API.Agent.LifeCycle.ServiceInfo
import Hercules.API.Logs.LogEntry (LogEntry)
import qualified Hercules.API.Logs.LogEntry as LogEntry
import Hercules.API.Logs.LogMessage (LogMessage)
import qualified Hercules.API.Logs.LogMessage as LogMessage
import Hercules.Agent.Sensitive
import qualified Hercules.Agent.Socket as Socket
import Hercules.Agent.Worker.Build (runBuild)
import qualified Hercules.Agent.Worker.Build.Logger as Logger
import Hercules.Agent.Worker.Effect (runEffect)
import Hercules.Agent.Worker.HerculesStore (nixStore, setBuilderCallback, withHerculesStore)
import Hercules.Agent.Worker.HerculesStore.Context (HerculesStore)
import Hercules.Agent.WorkerProtocol.Command
  ( Command,
  )
import qualified Hercules.Agent.WorkerProtocol.Command as Command
import qualified Hercules.Agent.WorkerProtocol.Command.Build as Build
import qualified Hercules.Agent.WorkerProtocol.Command.BuildResult as BuildResult
import qualified Hercules.Agent.WorkerProtocol.Command.Effect as Effect
import Hercules.Agent.WorkerProtocol.Command.Eval
  ( Eval,
  )
import qualified Hercules.Agent.WorkerProtocol.Command.Eval as Eval
import Hercules.Agent.WorkerProtocol.Event
  ( Event (Exception),
  )
import qualified Hercules.Agent.WorkerProtocol.Event as Event
import qualified Hercules.Agent.WorkerProtocol.Event.Attribute as Attribute
import qualified Hercules.Agent.WorkerProtocol.Event.AttributeError as AttributeError
import qualified Hercules.Agent.WorkerProtocol.LogSettings as LogSettings
import Hercules.CNix as CNix
import Hercules.CNix.Expr (Match (IsAttrs, IsString), NixAttrs, RawValue, autoCallFunction, evalArgs, evalFile, getAttrBool, getAttrList, getAttrs, getDrvFile, getRecurseForDerivations, getStringIgnoreContext, init, isDerivation, isFunctor, match, rawValueType, withEvalStateConduit)
import Hercules.CNix.Expr.Context (EvalState)
import qualified Hercules.CNix.Expr.Raw
import Hercules.CNix.Expr.Typed (Value)
import Hercules.CNix.Std.Vector (StdVector)
import qualified Hercules.CNix.Std.Vector as Std.Vector
import Hercules.CNix.Store.Context (NixStorePathWithOutputs)
import Hercules.Error
import Katip
import qualified Language.C.Inline.Cpp.Exceptions as C
import qualified Network.URI
import Protolude hiding (bracket, catch, evalState, wait, withAsync, yield)
import qualified System.Environment as Environment
import System.IO (BufferMode (LineBuffering), hSetBuffering)
import System.Posix.IO (dup, fdToHandle, stdError)
import System.Posix.Signals (Handler (Catch), installHandler, raiseSignal, sigINT, sigTERM)
import System.Timeout (timeout)
import UnliftIO.Async (wait, withAsync)
import UnliftIO.Exception (bracket, catch)
import Prelude ()
import qualified Prelude

data HerculesState = HerculesState
  { drvsCompleted :: TVar (Map StorePath (UUID, BuildResult.BuildStatus)),
    drvsInProgress :: IORef (Set StorePath),
    herculesStore :: Ptr (Ref HerculesStore),
    wrappedStore :: Store,
    shortcutChannel :: Chan (Maybe Event)
  }

data BuildException = BuildException
  { buildExceptionDerivationPath :: Text,
    buildExceptionDetail :: Maybe Text
  }
  deriving (Show, Typeable)

instance Exception BuildException

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  Hercules.CNix.Expr.init
  _ <- installHandler sigTERM (Catch $ raiseSignal sigINT) Nothing
  Logger.initLogger
  [options] <- Environment.getArgs
  let allOptions =
        Prelude.read options
          ++ [
               -- narinfo-cache-negative-ttl: Always try requesting narinfos because it may have been built in the meanwhile
               ("narinfo-cache-negative-ttl", "0"),
               -- Build concurrency is controlled by hercules-ci-agent, so set it
               -- to 1 to avoid accidentally consuming too many resources at once.
               ("max-jobs", "1")
             ]
  for_ allOptions $ \(k, v) -> do
    setGlobalOption k v
    setOption k v
  drvsCompleted_ <- newTVarIO mempty
  drvsInProgress_ <- newIORef mempty
  withStore $ \wrappedStore_ -> withHerculesStore wrappedStore_ $ \herculesStore_ -> withKatip $ do
    liftIO $ setBuilderCallback herculesStore_ mempty
    ch <- liftIO newChan
    let st =
          HerculesState
            { drvsCompleted = drvsCompleted_,
              drvsInProgress = drvsInProgress_,
              herculesStore = herculesStore_,
              wrappedStore = wrappedStore_,
              shortcutChannel = ch
            }
    let runner :: KatipContextT IO ()
        runner =
          ( ( do
                command <-
                  runConduitRes -- Res shouldn't be necessary
                    ( transPipe liftIO (sourceHandle stdin)
                        .| conduitDecode
                        .| printCommands
                        .| await
                    )
                    >>= \case
                      Just c -> pure c
                      Nothing -> panic "Not a valid starting command"
                runCommand st ch command
            )
              `safeLiftedCatch` ( \e -> liftIO $ do
                                    writeChan ch (Just $ Exception (renderException (e :: SomeException)))
                                    exitFailure
                                )
          )
            `EL.finally` ( do
                             liftIO $ writeChan ch Nothing
                             logLocM DebugS "runner done"
                         )
        writer =
          runConduitRes
            ( sourceChan ch
                .| conduitEncode
                .| concatMapC (\x -> [Chunk x, Flush])
                .| transPipe liftIO (sinkHandleFlush stdout)
            )
    void $
      withAsync runner $ \runnerAsync -> do
        writer -- runner can stop writer only by passing Nothing in channel (finally)
        logLocM DebugS "Writer done"
        wait runnerAsync -- include the potential exception

printCommands :: KatipContext m => ConduitT Command Command m ()
printCommands =
  mapMC
    ( \x -> do
        katipAddContext (sl "command" (show x :: Text)) $
          logLocM DebugS "Received command"
        pure x
    )

renderException :: SomeException -> Text
renderException e | Just (C.CppStdException msg) <- fromException e = toS msg
renderException e
  | Just (C.CppOtherException maybeType) <- fromException e =
    "Unexpected C++ exception" <> foldMap (\t -> " of type " <> toS t) maybeType
renderException e | Just (FatalError msg) <- fromException e = msg
renderException e = toS $ displayException e

connectCommand ::
  (MonadUnliftIO m, KatipContext m, MonadThrow m) =>
  Chan (Maybe Event) ->
  ConduitM Command Event (ResourceT m) () ->
  m ()
connectCommand ch conduit =
  runConduitRes
    ( sourceHandle stdin
        .| conduitDecode
        .| printCommands
        .| conduit
        .| sinkChan ch
    )

runCommand :: (MonadUnliftIO m, MonadBaseControl IO m, KatipContext m, MonadThrow m) => HerculesState -> Chan (Maybe Event) -> Command -> m ()
-- runCommand' :: HerculesState -> Command -> ConduitM Command Event (ResourceT IO) ()
runCommand herculesState ch command = do
  -- TODO don't do this
  mainThread <- liftIO myThreadId
  UnliftIO unlift <- askUnliftIO
  case command of
    Command.Eval eval -> Logger.withLoggerConduit (logger $ Eval.logSettings eval) $
      Logger.withTappedStderr Logger.tapper $
        connectCommand ch $ do
          void $
            liftIO $
              flip
                forkFinally
                (escalateAs \e -> FatalError $ "Failed to fork: " <> show e)
                $ unlift $
                  runConduitRes
                    ( Data.Conduit.handleC
                        ( \e -> do
                            yield $ Event.Error (renderException e)
                            liftIO $ throwTo mainThread e
                        )
                        ( do
                            runEval herculesState eval
                            liftIO $ throwTo mainThread ExitSuccess
                        )
                        .| sinkChanTerminate (shortcutChannel herculesState)
                    )
          awaitForever $ \case
            Command.BuildResult (BuildResult.BuildResult path attempt result) -> do
              katipAddContext (sl "path" path <> sl "result" (show result :: Text)) $
                logLocM
                  DebugS
                  "Received remote build result"
              storePath <- liftIO $ CNix.parseStorePath (wrappedStore herculesState) (encodeUtf8 path)
              liftIO $ atomically $ modifyTVar (drvsCompleted herculesState) (M.insert storePath (attempt, result))
            _ -> pass
    Command.Build build ->
      katipAddNamespace "Build" $
        Logger.withLoggerConduit (logger $ Build.logSettings build) $
          Logger.withTappedStderr Logger.tapper $
            connectCommand ch $ runBuild (wrappedStore herculesState) build
    Command.Effect effect ->
      katipAddNamespace "Effect" $
        Logger.withLoggerConduit (logger $ Effect.logSettings effect) $
          Logger.withTappedStderr Logger.tapper $
            connectCommand ch $ do
              runEffect (wrappedStore herculesState) effect >>= \case
                ExitSuccess -> yield $ Event.EffectResult 0
                ExitFailure n -> yield $ Event.EffectResult n
    _ ->
      panic "Not a valid starting command"

logger :: (MonadIO m, MonadUnliftIO m, KatipContext m) => LogSettings.LogSettings -> ConduitM () (Vector LogEntry) m () -> m ()
logger logSettings_ entriesSource = do
  socketConfig <- liftIO $ makeSocketConfig logSettings_
  let withPings socket m =
        withAsync
          ( liftIO $ forever do
              -- TODO add ping constructor to Frame or use websocket pings
              let ping = LogMessage.LogEntries mempty
              threadDelay 30_000_000
              atomically $ Socket.write socket ping
          )
          (const m)
  Socket.withReliableSocket socketConfig $ \socket -> withPings socket do
    let conduit =
          entriesSource
            .| Logger.unbatch
            .| Logger.filterProgress
            .| renumber 0
            .| batchAndEnd
            .| socketSink socket
        batch = Logger.batch .| mapC (LogMessage.LogEntries . V.fromList)
        batchAndEnd =
          (foldMapTap (Last . ims) `fuseUpstream` batch) >>= \case
            Last (Just (i, ms)) -> yield $ LogMessage.End {i = i + 1, ms = ms}
            Last Nothing -> yield $ LogMessage.End 0 0
          where
            ims (Chunk logEntry) = Just (LogEntry.i logEntry, LogEntry.ms logEntry)
            ims _ = Nothing
        renumber i =
          await >>= traverse_ \case
            Flush -> yield Flush >> renumber i
            Chunk e -> do
              yield $ Chunk e {LogEntry.i = i}
              renumber (i + 1)
    runConduit conduit
    logLocM DebugS "Syncing"
    liftIO (timeout 600_000_000 $ Socket.syncIO socket) >>= \case
      Just _ -> pass
      Nothing -> panic "Could not push logs within 10 minutes after completion"
    logLocM DebugS "Logger done"

socketSink :: MonadIO m => Socket.Socket r w -> ConduitT w o m ()
socketSink socket = awaitForever $ liftIO . atomically . Socket.write socket

-- | Perform a foldMap while yielding the original values ("tap").
--
-- '<>' is invoked with the new b on the right.
foldMapTap :: (Monoid b, Monad m) => (a -> b) -> ConduitT a a m b
foldMapTap f = go mempty
  where
    go b =
      await >>= \case
        Nothing -> pure b
        Just a -> do
          yield a
          go (b <> f a)

withKatip :: (MonadUnliftIO m) => KatipContextT m a -> m a
withKatip m = do
  let format :: forall a. LogItem a => ItemFormatter a
      format = (\_ _ _ -> "@katip ") <> jsonFormat
  -- Use a duplicate of stderr, to make sure we keep logging there, even after
  -- we reassign stderr to catch output from git and other subprocesses of Nix.
  dupStderr <- liftIO (fdToHandle =<< dup stdError)
  handleScribe <- liftIO $ mkHandleScribeWithFormatter format (ColorLog False) dupStderr (permitItem DebugS) V2
  let makeLogEnv = registerScribe "stderr" handleScribe defaultScribeSettings =<< initLogEnv "Worker" "production"
      initialContext = ()
      extraNs = mempty -- "Worker" is already set in initLogEnv.
      -- closeScribes will stop accepting new logs, flush existing ones and clean up resources
  bracket (liftIO makeLogEnv) (liftIO . closeScribes) $ \logEnv ->
    runKatipContextT logEnv initialContext extraNs m

makeSocketConfig :: Monad m => LogSettings.LogSettings -> IO (Socket.SocketConfig LogMessage Hercules.API.Agent.LifeCycle.ServiceInfo.ServiceInfo m)
makeSocketConfig l = do
  baseURL <- case Network.URI.parseURI $ toS $ LogSettings.baseURL l of
    Just x -> pure x
    Nothing -> panic "LogSettings: invalid base url"
  pure
    Socket.SocketConfig
      { makeHello = pure (LogMessage.LogEntries mempty),
        checkVersion = Socket.checkVersion',
        baseURL = baseURL,
        path = LogSettings.path l,
        token = encodeUtf8 $ reveal $ LogSettings.token l
      }

-- TODO: test
autoArgArgs :: Map Text Eval.Arg -> [ByteString]
autoArgArgs kvs = do
  (k, v) <- M.toList kvs
  case v of
    Eval.LiteralArg s -> ["--argstr", encodeUtf8 k, s]
    Eval.ExprArg s -> ["--arg", encodeUtf8 k, s]

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

yieldAttributeError :: Monad m => [ByteString] -> SomeException -> ConduitT i Event m ()
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
            AttributeError.errorType = Just "BuildException"
          }
yieldAttributeError path e =
  yield $
    Event.AttributeError $
      AttributeError.AttributeError
        { AttributeError.path = path,
          AttributeError.message = renderException e,
          AttributeError.errorDerivation = Nothing,
          AttributeError.errorType = Just (show (typeOf e))
        }

maybeThrowBuildException :: MonadIO m => BuildResult.BuildStatus -> Text -> m ()
maybeThrowBuildException result plainDrvText =
  case result of
    BuildResult.Failure -> throwIO $ BuildException plainDrvText Nothing
    BuildResult.DependencyFailure -> throwIO $ BuildException plainDrvText (Just "A dependency could not be built.")
    BuildResult.Success -> pass

runEval ::
  forall i m.
  (MonadResource m, KatipContext m, MonadUnliftIO m) =>
  HerculesState ->
  Eval ->
  ConduitM i Event m ()
runEval st@HerculesState {herculesStore = hStore, shortcutChannel = shortcutChan, drvsCompleted = drvsCompl} eval = do
  for_ (Eval.extraNixOptions eval) $ liftIO . uncurry setGlobalOption
  for_ (Eval.extraNixOptions eval) $ liftIO . uncurry setOption
  let store = nixStore hStore
  s <- storeUri store
  UnliftIO unlift <- lift askUnliftIO
  let decode = decodeUtf8With lenientDecode
  liftIO $
    setBuilderCallback hStore $
      traverseSPWOs $ \storePathWithOutputs -> unlift $ do
        drvStorePath <- liftIO $ getStorePath storePathWithOutputs
        drvPath <- liftIO $ CNix.storePathToPath store drvStorePath
        let pathText = decode drvPath
        outputs <- liftIO $ getOutputs storePathWithOutputs
        katipAddContext (sl "fullpath" pathText) $
          for_ outputs $ \outputName -> do
            logLocM DebugS "Building"
            withDrvInProgress st drvStorePath $ do
              liftIO $ writeChan shortcutChan $ Just $ Event.Build drvPath (decode outputName) Nothing
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
              katipAddContext (sl "outputPath" (show outputPath :: Text)) $
                logLocM DebugS "Naively calling ensurePath"
              liftIO (ensurePath (wrappedStore st) outputPath) `catch` \e0 -> do
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
                  liftIO $ writeChan shortcutChan $ Just $ Event.Build drvPath (decode outputName) (Just attempt0)
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
            logLocM DebugS "Built"
  withEvalStateConduit store $ \evalState -> do
    katipAddContext (sl "storeURI" (decode s)) $
      logLocM DebugS "EvalState loaded."
    args <-
      liftIO $
        evalArgs evalState (autoArgArgs (Eval.autoArguments eval))
    Data.Conduit.handleC (yieldAttributeError []) $
      do
        imprt <- liftIO $ evalFile evalState (toS $ Eval.file eval)
        applied <- liftIO (autoCallFunction evalState imprt args)
        walk store evalState args applied
    yield Event.EvaluationDone

traverseSPWOs :: (StorePathWithOutputs -> IO ()) -> StdVector NixStorePathWithOutputs -> IO ()
traverseSPWOs f v = do
  v' <- Std.Vector.toListFP v
  traverse_ f v'

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
      -- | If True, always walk this attribute set. Only True for the root.
      Bool ->
      -- | Attribute path
      [ByteString] ->
      -- | Depth of tree remaining
      Integer ->
      -- | Auto arguments to pass to (attrset-)functions
      Value NixAttrs ->
      -- | Current node of the walk
      RawValue ->
      -- | Program that performs the walk and emits 'Event's
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
                  then do
                    drvStorePath <- getDrvFile evalState v
                    drvPath <- liftIO $ CNix.storePathToPath store drvStorePath
                    typE <- runExceptT do
                      isEffect <- liftEitherAs Left =<< liftIO (getAttrBool evalState attrValue "isEffect")
                      case isEffect of
                        Just True -> throwE $ Right Attribute.Effect
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

liftEitherAs :: MonadError e m => (e0 -> e) -> Either e0 a -> m a
liftEitherAs f = liftEither . rmap
  where
    rmap (Left e) = Left (f e)
    rmap (Right a) = Right a
