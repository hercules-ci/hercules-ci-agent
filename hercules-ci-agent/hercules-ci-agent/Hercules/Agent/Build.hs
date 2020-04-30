module Hercules.Agent.Build where

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.Conduit.Combinators as Conduit
import Data.Conduit.Process (sourceProcessWithStreams)
import Data.IORef.Lifted
import qualified Data.Map as M
import qualified Hercules.API.Agent.Build as API.Build
import qualified Hercules.API.Agent.Build.BuildEvent as BuildEvent
import qualified Hercules.API.Agent.Build.BuildEvent.OutputInfo as OutputInfo
import Hercules.API.Agent.Build.BuildEvent.OutputInfo
  ( OutputInfo,
  )
import qualified Hercules.API.Agent.Build.BuildEvent.Pushed as Pushed
import qualified Hercules.API.Agent.Build.BuildTask as BuildTask
import Hercules.API.Agent.Build.BuildTask
  ( BuildTask,
  )
import qualified Hercules.API.Logs as API.Logs
import Hercules.API.Servant (noContent)
import Hercules.API.TaskStatus (TaskStatus)
import qualified Hercules.API.TaskStatus as TaskStatus
import qualified Hercules.Agent.Cachix as Agent.Cachix
import qualified Hercules.Agent.Client
import qualified Hercules.Agent.Config as Config
import Hercules.Agent.Env
import qualified Hercules.Agent.Env as Env
import Hercules.Agent.Log
import qualified Hercules.Agent.Nix as Nix
import Hercules.Agent.WorkerProcess
import qualified Hercules.Agent.WorkerProtocol.Command as Command
import qualified Hercules.Agent.WorkerProtocol.Command.Build as Command.Build
import qualified Hercules.Agent.WorkerProtocol.Event as Event
import qualified Hercules.Agent.WorkerProtocol.Event.BuildResult as BuildResult
import qualified Hercules.Agent.WorkerProtocol.LogSettings as LogSettings
import Hercules.Error (defaultRetry)
import qualified Katip.Core
import Protolude
import Servant.Auth.Client
import System.Process

performBuild :: BuildTask.BuildTask -> App TaskStatus
performBuild buildTask = do
  workerExe <- getWorkerExe
  commandChan <- liftIO newChan
  statusRef <- newIORef Nothing
  extraNixOptions <- Nix.askExtraOptions
  let opts = [show $ extraNixOptions]
      procSpec =
        (System.Process.proc workerExe opts)
          { env = Just [("NIX_PATH", "")],
            close_fds = True,
            cwd = Just "/"
          }
      writeEvent :: Event.Event -> App ()
      writeEvent event = case event of
        Event.BuildResult r -> writeIORef statusRef $ Just r
        Event.Exception e -> do
          logLocM DebugS $ show e
          panic e
        _ -> pass
  bulkSocketHost <- asks (Config.bulkSocketBase . Env.config)
  liftIO $ writeChan commandChan $ Just $ Command.Build $ Command.Build.Build
    { drvPath = BuildTask.derivationPath buildTask,
      inputDerivationOutputPaths = toS <$> BuildTask.inputDerivationOutputPaths buildTask,
      logSettings = LogSettings.LogSettings
        { token = LogSettings.Sensitive $ BuildTask.logToken buildTask,
          path = "/api/v1/logs/build/socket",
          host = bulkSocketHost
        }
    }
  exitCode <- runWorker procSpec stderrLineHandler commandChan writeEvent
  logLocM DebugS $ "Worker exit: " <> show exitCode
  case exitCode of
    ExitSuccess -> pass
    _ -> panic $ "Worker failed: " <> show exitCode
  status <- readIORef statusRef
  case status of
    Just (BuildResult.BuildSuccess {outputs = outs'}) -> do
      let outs = convertOutputs (BuildTask.derivationPath buildTask) outs'
      reportOutputInfos buildTask outs
      push buildTask outs
      reportSuccess buildTask
      pure $ TaskStatus.Successful ()
    Just (BuildResult.BuildFailure {}) -> pure $ TaskStatus.Terminated ()
    Nothing -> pure $ TaskStatus.Exceptional "Build did not complete"

convertOutputs :: Text -> [BuildResult.OutputInfo] -> Map Text OutputInfo
convertOutputs deriver = foldMap $ \oi ->
  M.singleton (toS $ BuildResult.name oi) $
    OutputInfo.OutputInfo
      { OutputInfo.deriver = deriver,
        name = toSL $ BuildResult.name oi,
        path = toSL $ BuildResult.path oi,
        size = fromIntegral $ BuildResult.size oi,
        hash = toSL $ BuildResult.hash oi
      }

stderrLineHandler :: Int -> ByteString -> App ()
stderrLineHandler _ ln
  | "@katip " `BS.isPrefixOf` ln,
    Just item <- A.decode (toS $ BS.drop 7 ln) =
    -- "This is the lowest level function [...] useful when implementing centralised logging services."
    Katip.Core.logKatipItem (Katip.Core.SimpleLogPayload . M.toList . fmap (Katip.Core.AnyLogPayload :: A.Value -> Katip.Core.AnyLogPayload) <$> item)
stderrLineHandler pid ln =
  withNamedContext "worker" (pid :: Int)
    $ logLocM InfoS
    $ "Builder: " <> logStr (toSL ln :: Text)

realise :: BuildTask.BuildTask -> App TaskStatus
realise buildTask = do
  let stdinc = pass
      stdoutc = pass -- FIXME: use
      stderrc = Conduit.fold
  nixStoreProc <-
    Nix.nixProc
      "nix-store"
      [ "--realise",
        "--timeout",
        "36000", -- 10h TODO: make configurable via meta.timeout and decrease default to 3600s or so
        "--max-silent-time",
        "1800" -- 0.5h TODO: make configurable via (?) and decrease default to 600s
      ]
      [ BuildTask.derivationPath buildTask
      ]
  let procSpec =
        nixStoreProc
          { close_fds = True, -- Disable on Windows?
            cwd = Just "/"
          }
  logLocM DebugS $ "Building: " <> show (System.Process.cmdspec procSpec)
  (status, _out, errBytes) <-
    liftIO $
      sourceProcessWithStreams procSpec stdinc stdoutc stderrc
  noContent $ defaultRetry $ runHerculesClient' $
    API.Logs.writeLog
      Hercules.Agent.Client.logsClient
      (Token $ toSL $ BuildTask.logToken buildTask)
      errBytes
  case status of
    ExitFailure e -> do
      withNamedContext "exitStatus" e $ logLocM ErrorS "Building failed"
      emitEvents buildTask [BuildEvent.Done False]
      pure $ TaskStatus.Terminated ()
    ExitSuccess -> TaskStatus.Successful () <$ logLocM DebugS "Building succeeded"

push :: BuildTask -> Map Text OutputInfo -> App ()
push buildTask outs = do
  let paths = OutputInfo.path <$> toList outs
  caches <- Agent.Cachix.activePushCaches
  forM_ caches $ \cache -> do
    Agent.Cachix.push cache paths 4
    emitEvents buildTask [BuildEvent.Pushed $ Pushed.Pushed {cache = cache}]

reportSuccess :: BuildTask -> App ()
reportSuccess buildTask = emitEvents buildTask [BuildEvent.Done True]

reportOutputInfos :: BuildTask -> Map Text OutputInfo -> App ()
reportOutputInfos buildTask outs =
  emitEvents buildTask $ map BuildEvent.OutputInfo (toList outs)

emitEvents :: BuildTask -> [BuildEvent.BuildEvent] -> App ()
emitEvents buildTask =
  noContent . defaultRetry . runHerculesClient
    . API.Build.updateBuild
      Hercules.Agent.Client.buildClient
      (BuildTask.id buildTask)
