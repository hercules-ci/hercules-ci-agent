module Hercules.Agent
  ( main,
  )
where

import Control.Concurrent.Async.Lifted
  ( race,
    replicateConcurrently_,
  )
import Control.Concurrent.MVar.Lifted (withMVar)
import Control.Exception (displayException)
import Control.Exception.Lifted (bracket)
import qualified Data.Aeson as A
import Data.Time (getCurrentTime)
import qualified Data.UUID.V4 as UUID
import qualified Hercules.API.Agent.Build.BuildTask as BuildTask
import qualified Hercules.API.Agent.Evaluate.EvaluateTask as EvaluateTask
import qualified Hercules.API.Agent.LifeCycle as LifeCycle
import qualified Hercules.API.Agent.LifeCycle.StartInfo as StartInfo
import Hercules.API.Agent.Tasks
  ( tasksReady,
    tasksSetStatus,
  )
import Hercules.API.Id (Id (Id))
import Hercules.API.Servant (noContent)
import Hercules.API.Task (Task)
import qualified Hercules.API.Task as Task
import qualified Hercules.API.TaskStatus as TaskStatus
import qualified Hercules.Agent.Build as Build
import Hercules.Agent.CabalInfo (herculesAgentVersion)
import qualified Hercules.Agent.Cachix as Cachix
import Hercules.Agent.Client
  ( lifeCycleClient,
    tasksClient,
  )
import qualified Hercules.Agent.Config as Config
import qualified Hercules.Agent.Env as Env
import Hercules.Agent.Env
  ( App,
    runHerculesClient,
  )
import Hercules.Agent.EnvironmentInfo (extractAgentInfo)
import qualified Hercules.Agent.Evaluate as Evaluate
import qualified Hercules.Agent.Init as Init
import Hercules.Agent.Log
import qualified Hercules.Agent.Options as Options
import Hercules.Agent.Token (withAgentToken)
import Hercules.Error
  ( cap,
    exponential,
    retry,
    safeLiftedHandle,
  )
import Protolude hiding
  ( bracket,
    handle,
    race,
    retry,
    withMVar,
  )

main :: IO ()
main = Init.setupLogging $ \logEnv -> do
  opts <- Options.parse
  let cfgPath = Options.configFile opts
  cfg <- Config.finalizeConfig cfgPath =<< Config.readConfig cfgPath
  env <- Init.newEnv cfg logEnv
  fetchTaskMutex <- newMVar ()
  Env.runApp env
    $ katipAddContext (sl "agent-version" (A.String herculesAgentVersion))
    $ withAgentToken
    $ withLifeCycle
    $ (logLocM InfoS "Agent online." >>)
    $ replicateConcurrently_ (fromIntegral $ Config.concurrentTasks cfg)
    $ forever
    $ do
      taskMaybe <-
        withMVar fetchTaskMutex
          $ const
          $ safeLiftedHandle
            ( \e -> do
                logLocM WarningS $
                  "Exception fetching task, retrying: "
                    <> show
                      (e :: SomeException)
                liftIO $ threadDelay (60 * 1000 * 1000)
                pure Nothing
            )
          $ ( >>=
                \case
                  Nothing -> do
                    liftIO $ threadDelay (1000 * 1000)
                    pure Nothing
                  x -> pure x
            )
          $ Env.runHerculesClient
          $ tasksReady tasksClient
      forM_ taskMaybe $ performTask

withLifeCycle :: App a -> App a
withLifeCycle app = do
  agentInfo <- extractAgentInfo
  now <- liftIO getCurrentTime
  freshId <- Id <$> liftIO UUID.nextRandom
  let startInfo = StartInfo.StartInfo {id = freshId, startTime = now}
      hello = StartInfo.Hello
        { agentInfo = agentInfo,
          startInfo = startInfo
        }
      req r =
        retry (cap 60 exponential)
          $ noContent
          $ runHerculesClient
          $ r
      sayHello = req $ LifeCycle.hello lifeCycleClient hello
      pinger = forever $ do
        req $ LifeCycle.heartbeat lifeCycleClient startInfo
        let oneSecond = 1000 * 1000
        liftIO $ threadDelay (oneSecond * 60)
      sayGoodbye = req $ LifeCycle.goodbye lifeCycleClient startInfo
      withPinger =
        fmap (either identity identity)
          . race pinger
  bracket sayHello (\() -> sayGoodbye) (\() -> withPinger app)

performTask :: Task Task.Any -> App ()
performTask task = contextually $ do
  result <- handleExceptions performTaskPerType
  logStatus result
  report result
  where
    contextually =
      withNamedContext "task" (Task.id task)
        . withNamedContext "task-type" (Task.typ task)
    handleExceptions m =
      safeLiftedHandle
        ( \e -> do
            pure $ TaskStatus.Exceptional (toSL $ displayException e)
        )
        m
    performTaskPerType = do
      logLocM InfoS "Starting task"
      case Task.typ task of
        "eval" -> do
          let evalTask :: Task EvaluateTask.EvaluateTask
              evalTask = Task.uncheckedCast task
          Cachix.withCaches $
            Evaluate.performEvaluation evalTask
          pure (TaskStatus.Successful ())
        "build" -> do
          let buildTask :: Task BuildTask.BuildTask
              buildTask = Task.uncheckedCast task
          Cachix.withCaches $
            Build.performBuild buildTask
        _ -> panicWithLog "Unknown task type"
    report status =
      retry (cap 60 exponential)
        $ noContent
        $ runHerculesClient
        $ tasksSetStatus tasksClient (Task.id task) status
    logStatus (TaskStatus.Terminated _) = logLocM InfoS "Completed failed task"
    logStatus (TaskStatus.Successful _) = logLocM InfoS "Completed task successfully"
    logStatus (TaskStatus.Exceptional e) = logLocM ErrorS $ "Exception in task: " <> logStr e
