module Hercules.Agent.Effect where

import Data.IORef
import qualified Hercules.API.Agent.Effect.EffectTask as EffectTask
import Hercules.API.TaskStatus (TaskStatus)
import qualified Hercules.API.TaskStatus as TaskStatus
import qualified Hercules.Agent.Config as Config
import Hercules.Agent.Env hiding (config)
import qualified Hercules.Agent.Env as Env
import Hercules.Agent.Files
import Hercules.Agent.Log
import qualified Hercules.Agent.Nix as Nix
import Hercules.Agent.Sensitive (Sensitive (Sensitive))
import qualified Hercules.Agent.ServiceInfo as ServiceInfo
import Hercules.Agent.WorkerProcess
import qualified Hercules.Agent.WorkerProcess as WorkerProcess
import qualified Hercules.Agent.WorkerProtocol.Command as Command
import qualified Hercules.Agent.WorkerProtocol.Command.Effect as Command.Effect
import qualified Hercules.Agent.WorkerProtocol.Event as Event
import qualified Hercules.Agent.WorkerProtocol.LogSettings as LogSettings
import qualified Network.URI
import Protolude
import System.FilePath ((</>))
import qualified System.Posix.Signals as PS
import System.Process

performEffect :: EffectTask.EffectTask -> App TaskStatus
performEffect effectTask = withWorkDir "effect" $ \workDir -> do
  workerExe <- getWorkerExe
  commandChan <- liftIO newChan
  extraNixOptions <- Nix.askExtraOptions
  workerEnv <- liftIO $ WorkerProcess.prepareEnv (WorkerProcess.WorkerEnvSettings {nixPath = mempty})
  effectResult <- liftIO $ newIORef Nothing
  let opts = [show $ extraNixOptions]
      procSpec =
        (System.Process.proc workerExe opts)
          { env = Just workerEnv,
            close_fds = True,
            cwd = Just workDir
          }
      writeEvent :: Event.Event -> App ()
      writeEvent event = case event of
        Event.EffectResult e -> do
          liftIO $ writeIORef effectResult (Just e)
        Event.Exception e -> do
          panic e
        _ -> pass
  config <- asks Env.config
  let materialize = not (Config.nixUserIsTrusted config)
  baseURL <- asks (ServiceInfo.bulkSocketBaseURL . Env.serviceInfo)
  liftIO $ writeChan commandChan $ Just $ Command.Effect $
    Command.Effect.Effect
      { drvPath = EffectTask.derivationPath effectTask,
        inputDerivationOutputPaths = toS <$> EffectTask.inputDerivationOutputPaths effectTask,
        logSettings =
          LogSettings.LogSettings
            { token = Sensitive $ EffectTask.logToken effectTask,
              path = "/api/v1/logs/build/socket",
              baseURL = toS $ Network.URI.uriToString identity baseURL ""
            },
        materializeDerivation = materialize,
        secretsPath = toS $ Config.staticSecretsDirectory config </> "secrets.json",
        token = Sensitive (EffectTask.token effectTask),
        apiBaseURL = Config.herculesApiBaseURL config
      }
  exitCode <- runWorker procSpec (stderrLineHandler "Effect worker") commandChan writeEvent
  logLocM DebugS $ "Worker exit: " <> show exitCode
  let showSig n | n == PS.sigABRT = " (Aborted)"
      showSig n | n == PS.sigBUS = " (Bus)"
      showSig n | n == PS.sigCHLD = " (Child)"
      showSig n | n == PS.sigFPE = " (Floating point exception)"
      showSig n | n == PS.sigHUP = " (Hangup)"
      showSig n | n == PS.sigILL = " (Illegal instruction)"
      showSig n | n == PS.sigINT = " (Interrupted)"
      showSig n | n == PS.sigKILL = " (Killed)"
      showSig n | n == PS.sigPIPE = " (Broken pipe)"
      showSig n | n == PS.sigQUIT = " (Quit)"
      showSig n | n == PS.sigSEGV = " (Segmentation fault)"
      showSig n | n == PS.sigTERM = " (Terminated)"
      showSig _ = ""
  case exitCode of
    ExitSuccess -> pass
    ExitFailure n -> panic $ "Effect worker failed with exit code " <> show n <> showSig (negate $ fromIntegral n)
  liftIO (readIORef effectResult) >>= \case
    Nothing -> pure $ TaskStatus.Exceptional "Effect worker terminated without reporting status"
    Just 0 -> pure $ TaskStatus.Successful ()
    Just n | n > 0 -> pure $ TaskStatus.Terminated ()
    Just n -> pure $ TaskStatus.Exceptional $ "Effect process exited with status code " <> show n <> showSig (negate $ fromIntegral n)
