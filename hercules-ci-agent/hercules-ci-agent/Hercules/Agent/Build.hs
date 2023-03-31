{-# LANGUAGE CPP #-}
module Hercules.Agent.Build where

import Data.Aeson qualified as A
import Data.IORef.Lifted
import Data.Map qualified as M
import Hercules.API.Agent.Build qualified as API.Build
import Hercules.API.Agent.Build.BuildEvent qualified as BuildEvent
import Hercules.API.Agent.Build.BuildEvent.OutputInfo
  ( OutputInfo,
  )
import Hercules.API.Agent.Build.BuildEvent.OutputInfo qualified as OutputInfo
import Hercules.API.Agent.Build.BuildEvent.Pushed qualified as Pushed
import Hercules.API.Agent.Build.BuildTask
  ( BuildTask,
  )
import Hercules.API.Agent.Build.BuildTask qualified as BuildTask
import Hercules.API.Servant (noContent)
import Hercules.API.TaskStatus (TaskStatus)
import Hercules.API.TaskStatus qualified as TaskStatus
import Hercules.Agent.Cache qualified as Agent.Cache
#if ! MIN_VERSION_cachix(1, 4, 0)
import Hercules.Agent.Cachix.Env qualified as Cachix.Env
#endif
import Hercules.Agent.Client qualified
import Hercules.Agent.Config qualified as Config
import Hercules.Agent.Env
import Hercules.Agent.Env qualified as Env
import Hercules.Agent.Log
import Hercules.Agent.Nix qualified as Nix
import Hercules.Agent.Sensitive (Sensitive (Sensitive))
import Hercules.Agent.ServiceInfo qualified as ServiceInfo
import Hercules.Agent.WorkerProcess
import Hercules.Agent.WorkerProcess qualified as WorkerProcess
import Hercules.Agent.WorkerProtocol.Command qualified as Command
import Hercules.Agent.WorkerProtocol.Command.Build qualified as Command.Build
import Hercules.Agent.WorkerProtocol.Event qualified as Event
import Hercules.Agent.WorkerProtocol.Event.BuildResult qualified as BuildResult
import Hercules.Agent.WorkerProtocol.LogSettings qualified as LogSettings
import Hercules.CNix.Store qualified as CNix
import Hercules.Error (defaultRetry)
import Network.URI qualified
import Protolude
import System.Process

performBuild :: BuildTask.BuildTask -> App TaskStatus
performBuild buildTask = do
  workerExe <- getWorkerExe
  commandChan <- liftIO newChan
  statusRef <- newIORef Nothing
  extraNixOptions <- Nix.askExtraOptions
  workerEnv <-
    liftIO $
      WorkerProcess.prepareEnv
        ( WorkerProcess.WorkerEnvSettings
            { nixPath = mempty,
              extraEnv = mempty
            }
        )
  let opts = [show extraNixOptions]
      procSpec =
        (System.Process.proc workerExe opts)
          { env = Just workerEnv,
            close_fds = True,
            cwd = Nothing
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
  liftIO $
    writeChan commandChan $
      Just $
        Command.Build $
          Command.Build.Build
            { drvPath = BuildTask.derivationPath buildTask,
              inputDerivationOutputPaths = encodeUtf8 <$> BuildTask.inputDerivationOutputPaths buildTask,
              logSettings =
                LogSettings.LogSettings
                  { token = Sensitive $ BuildTask.logToken buildTask,
                    path = "/api/v1/logs/build/socket",
                    baseURL = toS $ Network.URI.uriToString identity baseURL ""
                  },
              materializeDerivation = materialize
            }
  let stderrHandler =
        stderrLineHandler
          ( M.fromList
              [ ("taskId", A.toJSON (BuildTask.id buildTask)),
                ("derivationPath", A.toJSON (BuildTask.derivationPath buildTask))
              ]
          )
          "Builder"
  exitCode <- runWorker procSpec stderrHandler commandChan writeEvent
  logLocM DebugS $ "Worker exit: " <> logStr (show exitCode :: Text)
  case exitCode of
    ExitSuccess -> pass
    _ -> panic $ "Worker failed: " <> show exitCode
  status <- readIORef statusRef
  case status of
    Just BuildResult.BuildSuccess {outputs = outs'} -> do
      let outs = convertOutputs (BuildTask.derivationPath buildTask) outs'
      reportOutputInfos buildTask outs
#if MIN_VERSION_cachix(1, 4, 0)
      CNix.withStore $ \store -> push store buildTask outs
#else
      asks (Cachix.Env.store . Env.cachixEnv) >>= \store -> push store buildTask outs
#endif
      reportSuccess buildTask
      pure $ TaskStatus.Successful ()
    Just BuildResult.BuildFailure {} -> pure $ TaskStatus.Terminated ()
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

push :: CNix.Store -> BuildTask -> Map Text OutputInfo -> App ()
push store buildTask outs = do
  let paths = OutputInfo.path <$> toList outs
  caches <- activePushCaches
  forM_ caches $ \cache -> do
    -- TODO preserve StorePath instead
    storePaths <- liftIO $ for paths (CNix.parseStorePath store . encodeUtf8)
    Agent.Cache.push store cache storePaths 4
    emitEvents buildTask [BuildEvent.Pushed $ Pushed.Pushed {cache = cache}]

reportSuccess :: BuildTask -> App ()
reportSuccess buildTask = emitEvents buildTask [BuildEvent.Done True]

reportOutputInfos :: BuildTask -> Map Text OutputInfo -> App ()
reportOutputInfos buildTask outs =
  emitEvents buildTask $ map BuildEvent.OutputInfo (toList outs)

emitEvents :: BuildTask -> [BuildEvent.BuildEvent] -> App ()
emitEvents buildTask =
  noContent
    . defaultRetry
    . runHerculesClient
    . API.Build.updateBuild
      Hercules.Agent.Client.buildClient
      (BuildTask.id buildTask)
