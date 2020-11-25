module Hercules.Agent.Build where

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
import Hercules.API.Servant (noContent)
import Hercules.API.TaskStatus (TaskStatus)
import qualified Hercules.API.TaskStatus as TaskStatus
import qualified Hercules.Agent.Cache as Agent.Cache
import qualified Hercules.Agent.Client
import qualified Hercules.Agent.Config as Config
import Hercules.Agent.Env
import qualified Hercules.Agent.Env as Env
import Hercules.Agent.Log
import qualified Hercules.Agent.Nix as Nix
import Hercules.Agent.Sensitive (Sensitive (Sensitive))
import qualified Hercules.Agent.ServiceInfo as ServiceInfo
import Hercules.Agent.WorkerProcess
import qualified Hercules.Agent.WorkerProcess as WorkerProcess
import qualified Hercules.Agent.WorkerProtocol.Command as Command
import qualified Hercules.Agent.WorkerProtocol.Command.Build as Command.Build
import qualified Hercules.Agent.WorkerProtocol.Event as Event
import qualified Hercules.Agent.WorkerProtocol.Event.BuildResult as BuildResult
import qualified Hercules.Agent.WorkerProtocol.LogSettings as LogSettings
import Hercules.Error (defaultRetry)
import qualified Network.URI
import Protolude
import System.Process

performBuild :: BuildTask.BuildTask -> App TaskStatus
performBuild buildTask = do
  workerExe <- getWorkerExe
  commandChan <- liftIO newChan
  statusRef <- newIORef Nothing
  extraNixOptions <- Nix.askExtraOptions
  workerEnv <- liftIO $ WorkerProcess.prepareEnv (WorkerProcess.WorkerEnvSettings {nixPath = mempty})
  let opts = [show $ extraNixOptions]
      procSpec =
        (System.Process.proc workerExe opts)
          { env = Just workerEnv,
            close_fds = True,
            cwd = Just "/"
          }
      writeEvent :: Event.Event -> App ()
      writeEvent event = case event of
        Event.BuildResult r -> writeIORef statusRef $ Just r
        Event.Exception e -> do
          logLocM DebugS $ logStr (show e :: Text)
          panic e
        _ -> pass
  baseURL <- asks (ServiceInfo.bulkSocketBaseURL . Env.serviceInfo)
  materialize <- asks (not . Config.nixUserIsTrusted . Env.config)
  liftIO $ writeChan commandChan $ Just $ Command.Build $
    Command.Build.Build
      { drvPath = BuildTask.derivationPath buildTask,
        inputDerivationOutputPaths = encodeUtf8 <$> BuildTask.inputDerivationOutputPaths buildTask,
        logSettings =
          LogSettings.LogSettings
            { token = LogSettings.Sensitive $ BuildTask.logToken buildTask,
              path = "/api/v1/logs/build/socket",
              baseURL = toS $ Network.URI.uriToString identity baseURL ""
            },
        materializeDerivation = materialize
      }
  exitCode <- runWorker procSpec (stderrLineHandler "Builder") commandChan writeEvent
  logLocM DebugS $ "Worker exit: " <> logStr (show exitCode :: Text)
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
  M.singleton (decodeUtf8With lenientDecode $ BuildResult.name oi) $
    OutputInfo.OutputInfo
      { OutputInfo.deriver = deriver,
        name = decodeUtf8With lenientDecode $ BuildResult.name oi,
        path = decodeUtf8With lenientDecode $ BuildResult.path oi,
        size = fromIntegral $ BuildResult.size oi,
        hash = decodeUtf8With lenientDecode $ BuildResult.hash oi
      }

push :: BuildTask -> Map Text OutputInfo -> App ()
push buildTask outs = do
  let paths = OutputInfo.path <$> toList outs
  caches <- activePushCaches
  forM_ caches $ \cache -> do
    Agent.Cache.push cache paths 4
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
