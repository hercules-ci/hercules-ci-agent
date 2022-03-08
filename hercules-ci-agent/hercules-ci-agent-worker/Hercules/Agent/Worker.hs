{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}

module Hercules.Agent.Worker
  ( main,
  )
where

import Conduit
import Control.Concurrent.STM hiding (check)
import qualified Control.Exception.Lifted as EL
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control
import Data.Coerce (coerce)
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
import Hercules.API.Agent.Evaluate.EvaluateEvent.OnPushHandlerEvent (OnPushHandlerEvent (OnPushHandlerEvent))
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.OnPushHandlerEvent
import qualified Hercules.API.Agent.Evaluate.EvaluateTask as EvaluateTask
import qualified Hercules.API.Agent.Evaluate.ImmutableGitInput as API.ImmutableGitInput
import qualified Hercules.API.Agent.Evaluate.ImmutableInput as API.ImmutableInput
import qualified Hercules.API.Agent.LifeCycle.ServiceInfo
import Hercules.API.Logs.LogEntry (LogEntry)
import qualified Hercules.API.Logs.LogEntry as LogEntry
import Hercules.API.Logs.LogHello (LogHello (LogHello, clientProtocolVersion, storeProtocolVersion))
import Hercules.API.Logs.LogMessage (LogMessage)
import qualified Hercules.API.Logs.LogMessage as LogMessage
import Hercules.Agent.NixFile (HerculesCISchema, getHerculesCI, homeExprRawValue, loadNixFile, parseExtraInputs)
import Hercules.Agent.NixFile.HerculesCIArgs (HerculesCIMeta (HerculesCIMeta), fromGitSource)
import qualified Hercules.Agent.NixFile.HerculesCIArgs
import Hercules.Agent.Sensitive
import qualified Hercules.Agent.Socket as Socket
import Hercules.Agent.Worker.Build (runBuild)
import qualified Hercules.Agent.Worker.Build.Logger as Logger
import Hercules.Agent.Worker.Effect (runEffect)
import Hercules.Agent.Worker.HerculesStore (nixStore, setBuilderCallback, withHerculesStore)
import Hercules.Agent.Worker.HerculesStore.Context (HerculesStore)
import Hercules.Agent.Worker.Logging (withKatip)
import Hercules.Agent.Worker.NixDaemon (nixDaemon)
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
    ViaJSON (ViaJSON),
  )
import qualified Hercules.Agent.WorkerProtocol.Event as Event
import qualified Hercules.Agent.WorkerProtocol.Event.Attribute as Attribute
import qualified Hercules.Agent.WorkerProtocol.Event.AttributeError as AttributeError
import qualified Hercules.Agent.WorkerProtocol.LogSettings as LogSettings
import Hercules.CNix as CNix
import Hercules.CNix.Expr (Match (IsAttrs, IsString), NixAttrs, RawValue, addAllowedPath, addInternalAllowedPaths, autoCallFunction, evalArgs, getAttrBool, getAttrList, getAttrs, getDrvFile, getFlakeFromArchiveUrl, getFlakeFromGit, getRecurseForDerivations, getStringIgnoreContext, init, isDerivation, isFunctor, match, rawValueType, rtValue, toRawValue, withEvalStateConduit)
import Hercules.CNix.Expr.Context (EvalState)
import qualified Hercules.CNix.Expr.Raw
import Hercules.CNix.Expr.Schema (MonadEval, PSObject, dictionaryToMap, fromPSObject, requireDict, (#.), (#?), (#?!), ($?))
import qualified Hercules.CNix.Expr.Schema as Schema
import Hercules.CNix.Expr.Typed (Value)
import Hercules.CNix.Std.Vector (StdVector)
import qualified Hercules.CNix.Std.Vector as Std.Vector
import Hercules.CNix.Store.Context (NixStorePathWithOutputs)
import Hercules.CNix.Util (installDefaultSigINTHandler)
import Hercules.Error
import Hercules.UserException (UserException (UserException))
import Katip
import qualified Language.C.Inline.Cpp.Exceptions as C
import qualified Network.URI
import Protolude hiding (bracket, catch, check, evalState, wait, withAsync, yield)
import qualified System.Environment as Environment
import System.IO (BufferMode (LineBuffering), hSetBuffering)
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
    shortcutChannel :: Chan (Maybe Event),
    extraNixOptions :: [(Text, Text)]
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
  installDefaultSigINTHandler
  Logger.initLogger
  args <- Environment.getArgs
  case args of
    ["nix-daemon", options] -> do
      setOptions options
      nixDaemon
    [options] -> taskWorker options
    _ -> throwIO $ FatalError "worker: Unrecognized command line arguments"

-- TODO Make this part of the worker protocol instead
parseOptions :: (Read a, Read b, IsString a, IsString b) => Prelude.String -> [(a, b)]
parseOptions options =
  Prelude.read options
    ++ [
         -- narinfo-cache-negative-ttl: Always try requesting narinfos because it may have been built in the meanwhile
         ("narinfo-cache-negative-ttl", "0"),
         -- Build concurrency is controlled by hercules-ci-agent, so set it
         -- to 1 to avoid accidentally consuming too many resources at once.
         ("max-jobs", "1")
       ]

setOptions :: [Char] -> IO ()
setOptions options = do
  for_ (parseOptions options) $ \(k, v) -> do
    setGlobalOption k v
    setOption k v

taskWorker :: [Char] -> IO ()
taskWorker options = do
  setOptions options
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
              shortcutChannel = ch,
              extraNixOptions = parseOptions options
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
  protocolVersion <- liftIO do
    getStoreProtocolVersion (wrappedStore herculesState)
  case command of
    Command.Eval eval -> Logger.withLoggerConduit (logger (Eval.logSettings eval) protocolVersion) $
      Logger.withTappedStderr Logger.tapper $
        connectCommand ch $ do
          liftIO $ restrictEval eval
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
        Logger.withLoggerConduit (logger (Build.logSettings build) protocolVersion) $
          Logger.withTappedStderr Logger.tapper $
            connectCommand ch $ runBuild (wrappedStore herculesState) build
    Command.Effect effect ->
      katipAddNamespace "Effect" $
        Logger.withLoggerConduit (logger (Effect.logSettings effect) protocolVersion) $
          Logger.withTappedStderr Logger.tapper $
            connectCommand ch $ do
              runEffect (extraNixOptions herculesState) (wrappedStore herculesState) effect >>= \case
                ExitSuccess -> yield $ Event.EffectResult 0
                ExitFailure n -> yield $ Event.EffectResult n
    _ ->
      panic "Not a valid starting command"

restrictEval :: Eval -> IO ()
restrictEval eval = do
  setGlobalOption "restrict-eval" "true"
  setGlobalOption "allowed-uris" $
    if Eval.allowInsecureBuiltinFetchers eval
      then allSchemes
      else safeSchemes
  where
    safeSchemes = "ssh:// https://"
    allSchemes = safeSchemes <> " http:// git://"

logger :: (MonadIO m, MonadUnliftIO m, KatipContext m) => LogSettings.LogSettings -> Int -> ConduitM () (Vector LogEntry) m () -> m ()
logger logSettings_ storeProtocolVersionValue entriesSource = do
  socketConfig <- liftIO $ makeSocketConfig logSettings_ storeProtocolVersionValue
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

makeSocketConfig :: MonadIO m => LogSettings.LogSettings -> Int -> IO (Socket.SocketConfig LogMessage Hercules.API.Agent.LifeCycle.ServiceInfo.ServiceInfo m)
makeSocketConfig l storeProtocolVersionValue = do
  clientProtocolVersionValue <- liftIO getClientProtocolVersion
  baseURL <- case Network.URI.parseURI $ toS $ LogSettings.baseURL l of
    Just x -> pure x
    Nothing -> panic "LogSettings: invalid base url"
  pure
    Socket.SocketConfig
      { makeHello =
          pure $
            LogMessage.Hello
              LogHello
                { clientProtocolVersion = clientProtocolVersionValue,
                  storeProtocolVersion = storeProtocolVersionValue
                },
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
  (MonadResource m, KatipContext m, MonadUnliftIO m, MonadThrow m) =>
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
        homeExpr <- escalateAs UserException =<< liftIO (loadNixFile evalState (toS $ Eval.cwd eval) (coerce $ Eval.gitSource eval))
        let hargs = fromGitSource (coerce $ Eval.gitSource eval) meta
            meta = HerculesCIMeta {apiBaseUrl = Eval.apiBaseUrl eval}
        liftIO (flip runReaderT evalState $ getHerculesCI homeExpr hargs) >>= \case
          Nothing ->
            -- legacy
            walk store evalState args (homeExprRawValue homeExpr)
          Just herculesCI -> do
            case Event.fromViaJSON (Eval.selector eval) of
              EvaluateTask.ConfigOrLegacy -> do
                yield Event.JobConfig
                sendConfig evalState herculesCI
              EvaluateTask.OnPush onPush ->
                transPipe (`runReaderT` evalState) do
                  walkOnPush store evalState onPush herculesCI
    yield Event.EvaluationDone

walkOnPush :: (MonadEval m, MonadUnliftIO m, KatipContext m, MonadThrow m) => Store -> Ptr EvalState -> EvaluateTask.OnPush -> PSObject HerculesCISchema -> ConduitT i Event m ()
walkOnPush store evalState onPushParams herculesCI = do
  onPushHandler <- herculesCI #?! #onPush >>= requireDict (EvaluateTask.name onPushParams)
  inputs <- liftIO $ do
    inputs <- for (EvaluateTask.inputs onPushParams) \input -> do
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

sendConfig :: MonadIO m => Ptr EvalState -> PSObject HerculesCISchema -> ConduitT i Event m ()
sendConfig evalState herculesCI = flip runReaderT evalState $ do
  herculesCI #? #onPush >>= traverse_ \onPushes -> do
    attrs <- dictionaryToMap onPushes
    for_ (M.mapWithKey (,) attrs) \(name, onPush) -> do
      ei <- onPush #? #extraInputs >>= traverse parseExtraInputs
      enable <- onPush #? #enable >>= traverse fromPSObject <&> fromMaybe True
      when enable . lift . yield . Event.OnPushHandler . ViaJSON $
        OnPushHandlerEvent
          { handlerName = decodeUtf8 name,
            handlerExtraInputs = M.mapKeys decodeUtf8 (fromMaybe mempty ei)
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
      -- | Attribute path
      [ByteString] ->
      -- | Depth of tree remaining
      Integer ->
      -- | Current node of the walk
      RawValue ->
      -- | Program that performs the walk and emits 'Event's
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
