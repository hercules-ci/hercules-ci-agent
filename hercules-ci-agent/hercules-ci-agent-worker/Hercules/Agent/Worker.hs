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
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Data.Aeson qualified as A
import Data.Binary qualified as B
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.Conduit qualified
import Data.Conduit.Katip.Orphans ()
import Data.Conduit.Serialization.Binary
  ( conduitDecode,
  )
import Data.IORef
import Data.Map qualified as M
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Natural (Natural)
import Hercules.Agent.Worker.Build (runBuild)
import Hercules.Agent.Worker.Build.Logger qualified as Logger
import Hercules.Agent.Worker.Effect (runEffect)
import Hercules.Agent.Worker.Env (HerculesState (HerculesState, drvsCompleted, wrappedStore))
import Hercules.Agent.Worker.Env qualified
import Hercules.Agent.Worker.Error (ExceptionText (exceptionTextMessage), exceptionTextMessage, renderException)
import Hercules.Agent.Worker.Evaluate (runEval)
import Hercules.Agent.Worker.HerculesStore (setBuilderCallback, withHerculesStore)
import Hercules.Agent.Worker.Logging (withKatip)
import Hercules.Agent.WorkerProtocol.Command
  ( Command,
  )
import Hercules.Agent.WorkerProtocol.Command qualified as Command
import Hercules.Agent.WorkerProtocol.Command.BuildResult qualified as BuildResult
import Hercules.Agent.WorkerProtocol.Command.Eval
  ( Eval,
  )
import Hercules.Agent.WorkerProtocol.Command.Eval qualified as Eval
import Hercules.Agent.WorkerProtocol.Event
  ( Event (Exception),
  )
import Hercules.Agent.WorkerProtocol.Event qualified as Event
import Hercules.Agent.WorkerProtocol.ViaJSON (ViaJSON (ViaJSON))
import Hercules.Agent.WorkerProtocol.WorkerConfig (WorkerConfig)
import Hercules.Agent.WorkerProtocol.WorkerConfig qualified
import Hercules.CNix as CNix
import Hercules.CNix.Expr (allowThreads, init, runGcRegisteredThread, setExtraStackOverflowHandlerToSleep)
import Hercules.CNix.Util (installDefaultSigINTHandler)
import Hercules.CNix.Verbosity (setShowTrace, setVerbosity)
import Hercules.CNix.Verbosity qualified as CNix.Verbosity
import Hercules.Error
import Katip
import Protolude hiding (bracket, catch, check, evalState, wait, withAsync, yield)
import System.IO (BufferMode (NoBuffering), hFlush, hSetBuffering)
import System.Posix.Signals (Handler (Catch), installHandler, raiseSignal, sigINT, sigTERM)
import UnliftIO.Async (wait, withAsync)

main :: IO ()
main = do
  parentHandles <- Logger.plumbWorkerStd
  withEventWriter parentHandles.events \sendEvents -> do
    withNixLogger sendEvents do
      Hercules.CNix.Expr.init
      Hercules.CNix.Expr.allowThreads
      setShowTrace True
      _ <- installHandler sigTERM (Catch $ raiseSignal sigINT) Nothing
      installDefaultSigINTHandler
      setExtraStackOverflowHandlerToSleep
      workerConfigBytes <- BSC.hGetLine parentHandles.commands
      workerConfig <- case A.eitherDecode (toS workerConfigBytes) of
        Left e -> panic ("Could not parse WorkerConfig: " <> show e)
        Right r -> pure r
      taskWorker parentHandles.commands sendEvents workerConfig

withNixLogger ::
  (Vector Event -> IO ()) ->
  IO r ->
  IO r
withNixLogger sendEvents work = do
  Logger.initLogger
  Logger.withLoggerConduit logger work
  where
    logger c =
      runConduit $
        c .| awaitForever \events -> do
          liftIO . sendEvents . pure . Event.LogItems . ViaJSON $ events

-- | Run a pipe. Wait for producer to complete and then wait for consumer to exit.
withProducerDrivenBoundedQueue ::
  -- Max queue size
  Natural ->
  (forall x. TBQueue (Either x b) -> IO r) ->
  (forall x. TBQueue (Either x b) -> IO x) ->
  IO r
withProducerDrivenBoundedQueue maxQueueSize producer consumer = do
  q <- newTBQueueIO maxQueueSize
  let produce = do
        ( do
            r <- producer q
            atomically do
              writeTBQueue q (Left r)
          )
          `onException` do
            atomically do
              writeTBQueue q (Left (panic "producer threw an exception"))

      consume = \producerDone -> do
        consumer q `finally` wait producerDone
  withAsync produce consume

withEventWriter :: Handle -> ((Vector Event -> IO ()) -> IO r) -> IO r
withEventWriter h f = do
  hSetBuffering h NoBuffering
  withProducerDrivenBoundedQueue 100 source shovel
  where
    source q = f (atomically . writeTBQueue q . Right)
    shovel q = do
      atomically (readTBQueue q) >>= \case
        Left r -> pure r
        Right events -> do
          unless (V.null events) do
            for_ events (BL.hPut h . B.encode)
            hFlush h
          shovel q

setOptions :: [(Text, Text)] -> IO ()
setOptions = traverse_ \(k, v) -> do
  setGlobalOption k v
  setOption k v

taskWorker :: Handle -> (Vector Event -> IO ()) -> WorkerConfig -> IO ()
taskWorker commandsHandle sendEvents_ cfg = do
  setOptions cfg.nixOptions
  -- at least info messages are expected in the dashboard logs
  let nixVerbosity = CNix.Verbosity.both CNix.Verbosity.Info cfg.nixVerbosity.unViaShowRead
      withKatip' = withKatip cfg.verbosity.unViaShowRead
  setVerbosity nixVerbosity
  drvsCompleted_ <- newTVarIO mempty
  drvBuildAsyncs_ <- newTVarIO mempty
  drvRebuildAsyncs_ <- newTVarIO mempty
  drvOutputSubstituteAsyncs_ <- newTVarIO mempty
  drvsInProgress_ <- newIORef mempty
  withStore $ \wrappedStore_ -> withHerculesStore wrappedStore_ $ \herculesStore_ -> withKatip' do
    liftIO $ setBuilderCallback herculesStore_ mempty
    let st =
          HerculesState
            { drvsCompleted = drvsCompleted_,
              drvBuildAsyncs = drvBuildAsyncs_,
              drvRebuildAsyncs = drvRebuildAsyncs_,
              drvOutputSubstituteAsyncs = drvOutputSubstituteAsyncs_,
              drvsInProgress = drvsInProgress_,
              herculesStore = herculesStore_,
              wrappedStore = wrappedStore_,
              sendEvents = sendEvents_,
              extraNixOptions = cfg.nixOptions,
              nixVerbosity = cfg.nixVerbosity.unViaShowRead
            }
    runConduitRes do
      sourceHandle commandsHandle
        .| conduitDecode
        .| printCommands
        .| do
          command <-
            await >>= \case
              Just c -> pure c
              Nothing -> panic "No starting command"
          runCommand st command
        `catchC` ( \e -> do
                     textual <- liftIO (renderException e)
                     yield (Exception $ exceptionTextMessage textual)
                     liftIO exitFailure
                 )
        .| awaitForever \x -> liftIO do sendEvents_ (pure x)

printCommands :: (KatipContext m) => ConduitT Command Command m ()
printCommands =
  mapMC
    ( \x -> do
        katipAddContext (sl "command" (show x :: Text)) $
          logLocM DebugS "Received command"
        pure x
    )

runCommand :: (MonadUnliftIO m, KatipContext m, MonadThrow m) => HerculesState -> Command -> ConduitM Command Event m ()
runCommand herculesState command = do
  -- TODO don't do this
  mainThread <- liftIO myThreadId

  case command of
    Command.Eval eval -> do
      liftIO $ restrictEval eval
      void $ lift do
        UnliftIO unlift <- askUnliftIO
        liftIO do
          flip
            forkFinally
            (escalateAs \e -> FatalError $ "Failed to fork: " <> show e)
            $ runGcRegisteredThread
            $ unlift
            $ runConduitRes
              ( Data.Conduit.handleC
                  ( \e -> do
                      yield . Event.Error . exceptionTextMessage =<< liftIO (renderException e)
                      liftIO $ throwTo mainThread e
                  )
                  ( do
                      runEval herculesState eval
                      liftIO $ throwTo mainThread ExitSuccess
                  )
                  .| awaitForever (liftIO . herculesState.sendEvents . pure)
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
        runBuild (wrappedStore herculesState) build
    Command.Effect effect ->
      katipAddNamespace "Effect" $
        runEffect (herculesState.extraNixOptions) (wrappedStore herculesState) effect >>= \case
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
    safeSchemes = "ssh:// https:// git+ssh:// git+https:// github: gitlab: sourcehut:"
    allSchemes = safeSchemes <> " http:// git://"
