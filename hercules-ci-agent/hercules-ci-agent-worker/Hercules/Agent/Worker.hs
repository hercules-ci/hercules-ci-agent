{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Data.Conduit
import Data.Conduit.Extras (sinkChan, sinkChanTerminate, sourceChan)
import Data.Conduit.Katip.Orphans ()
import Data.Conduit.Serialization.Binary
  ( conduitDecode,
    conduitEncode,
  )
import Data.IORef
import qualified Data.Map as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Hercules.API.Agent.LifeCycle.ServiceInfo
import Hercules.API.Logs.LogEntry (LogEntry)
import qualified Hercules.API.Logs.LogEntry as LogEntry
import Hercules.API.Logs.LogHello (LogHello (LogHello, clientProtocolVersion, storeProtocolVersion))
import Hercules.API.Logs.LogMessage (LogMessage)
import qualified Hercules.API.Logs.LogMessage as LogMessage
import Hercules.Agent.Sensitive
import qualified Hercules.Agent.Socket as Socket
import Hercules.Agent.Worker.Build (runBuild)
import qualified Hercules.Agent.Worker.Build.Logger as Logger
import Hercules.Agent.Worker.Effect (runEffect)
import Hercules.Agent.Worker.Env (HerculesState (..))
import Hercules.Agent.Worker.Error (ExceptionText (exceptionTextMessage), exceptionTextMessage, renderException)
import Hercules.Agent.Worker.Evaluate (runEval)
import Hercules.Agent.Worker.HerculesStore (setBuilderCallback, withHerculesStore)
import Hercules.Agent.Worker.Logging (withKatip)
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
import qualified Hercules.Agent.WorkerProtocol.LogSettings as LogSettings
import Hercules.CNix as CNix
import Hercules.CNix.Expr (init)
import Hercules.CNix.Util (installDefaultSigINTHandler)
import Hercules.CNix.Verbosity (setShowTrace)
import Hercules.Error
import Katip
import qualified Network.URI
import Protolude hiding (bracket, catch, check, evalState, wait, withAsync, yield)
import qualified System.Environment as Environment
import System.IO (BufferMode (LineBuffering), hSetBuffering)
import System.Posix.Signals (Handler (Catch), installHandler, raiseSignal, sigINT, sigTERM)
import System.Timeout (timeout)
import UnliftIO.Async (wait, withAsync)
import Prelude ()
import qualified Prelude

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  Hercules.CNix.Expr.init
  setShowTrace True
  _ <- installHandler sigTERM (Catch $ raiseSignal sigINT) Nothing
  installDefaultSigINTHandler
  Logger.initLogger
  args <- Environment.getArgs
  case args of
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
  drvBuildAsyncs_ <- newTVarIO mempty
  drvRebuildAsyncs_ <- newTVarIO mempty
  drvOutputSubstituteAsyncs_ <- newTVarIO mempty
  drvsInProgress_ <- newIORef mempty
  withStore $ \wrappedStore_ -> withHerculesStore wrappedStore_ $ \herculesStore_ -> withKatip $ do
    liftIO $ setBuilderCallback herculesStore_ mempty
    ch <- liftIO newChan
    let st =
          HerculesState
            { drvsCompleted = drvsCompleted_,
              drvBuildAsyncs = drvBuildAsyncs_,
              drvRebuildAsyncs = drvRebuildAsyncs_,
              drvOutputSubstituteAsyncs = drvOutputSubstituteAsyncs_,
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
                                    textual <- renderException e
                                    writeChan ch (Just $ Exception $ exceptionTextMessage textual)
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
                            yield . Event.Error . exceptionTextMessage =<< liftIO (renderException e)
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
