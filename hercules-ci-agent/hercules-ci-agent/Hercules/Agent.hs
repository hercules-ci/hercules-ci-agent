module Hercules.Agent
  ( main
  )
where

import           Protolude               hiding ( handle
                                                , withMVar
                                                , retry
                                                )

import           Control.Concurrent.Async.Lifted
                                                ( replicateConcurrently_ )
import           Control.Concurrent.MVar.Lifted ( withMVar )

import           Hercules.API                   ( noContent )
import           Hercules.API.Agent.Tasks       ( tasksReady
                                                , tasksSetStatus
                                                )
import           Hercules.API.Task              ( Task )
import qualified Hercules.API.Task             as Task
import qualified Hercules.API.TaskStatus       as TaskStatus
import qualified Hercules.API.Agent.Evaluate.EvaluateTask
                                               as EvaluateTask
import qualified Hercules.API.Agent.Build.BuildTask
                                               as BuildTask
import           Hercules.Agent.CabalInfo       ( herculesAgentVersion )
import           Hercules.Agent.Client          ( tasksClient )
import           Hercules.Agent.Token           ( withAgentToken )
import qualified Hercules.Agent.Evaluate       as Evaluate
import qualified Hercules.Agent.Env            as Env
import qualified Hercules.Agent.Options        as Options
import qualified Hercules.Agent.Config         as Config
import           Hercules.Agent.Env             ( App
                                                , runHerculesClient
                                                )
import qualified Hercules.Agent.Init           as Init
import qualified Hercules.Agent.Build          as Build
import           Hercules.Agent.Exception       ( safeLiftedHandle
                                                , retry
                                                , exponential
                                                , cap
                                                )
import           Hercules.Agent.Scribe          ( withHerculesScribe )

import           Hercules.Agent.Log
import qualified Data.Aeson                    as A

main :: IO ()
main = Init.setupLogging $ \logEnv -> do
  opts <- Options.parse
  cfg <- Config.readConfig (toS <$> Options.configFile opts)
  env <- Init.newEnv (Options.configOverrides opts `appEndo` cfg) logEnv

  fetchTaskMutex <- newMVar ()

  Env.runApp env
    $ katipAddContext (sl "agent-version" (A.String herculesAgentVersion))
    $ withAgentToken
    $ withHerculesScribe
    $ (logLocM InfoS "Agent online." >>)
    $ replicateConcurrently_ (fromIntegral $ Config.concurrentTasks cfg)
    $ forever
    $ do
        taskMaybe <-
          withMVar fetchTaskMutex
          $ const
          $ safeLiftedHandle
              (\e -> do
                logLocM WarningS $ "Exception fetching task, retrying: " <> show
                  (e :: SomeException)
                liftIO $ threadDelay (60 * 1000 * 1000)
                pure Nothing
              )
          $ (>>= \case
              Nothing -> do
                liftIO $ threadDelay (1000 * 1000)
                pure Nothing
              x -> pure x
            )
          $ Env.runHerculesClient
          $ tasksReady tasksClient

        forM_ taskMaybe $ performTask

performTask :: Task Task.Any -> App ()
performTask task =
  withNamedContext "task" (Task.id task)
    $ withNamedContext "task-type" (Task.typ task)
    $ safeLiftedHandle
        (\e -> do
          logLocM ErrorS $ "Exception in task: " <> show (e :: SomeException)
          retry (cap 60 exponential)
            $ noContent
            $ runHerculesClient
            $ tasksSetStatus tasksClient
                             (Task.id task)
                             (TaskStatus.Exceptional $ show e)
        )
    $ do
        logLocM InfoS "Starting task"

        case Task.typ task of
          "eval" -> do
            let evalTask :: Task EvaluateTask.EvaluateTask
                evalTask = Task.uncheckedCast task
            Evaluate.performEvaluation evalTask
          "build" -> do
            let buildTask :: Task BuildTask.BuildTask
                buildTask = Task.uncheckedCast task
            Build.performBuild buildTask
          _ -> panicWithLog "Unknown task type"

        logLocM InfoS "Completed task successfully"

        noContent $ runHerculesClient $ tasksSetStatus
          tasksClient
          (Task.id task)
          (TaskStatus.Successful ())
