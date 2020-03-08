{-# LANGUAGE BlockArguments #-}

module Hercules.Agent
  ( main,
  )
where

import Control.Concurrent.Async.Lifted
  ( race,
  )
import Control.Concurrent.Lifted (forkFinally, killThread)
import Control.Concurrent.STM.TChan
import Control.Exception (displayException)
import Control.Exception.Lifted (bracket)
import qualified Data.Aeson as A
import Data.IORef.Lifted
import qualified Data.Map as M
import Data.Time (getCurrentTime)
import qualified Data.UUID.V4 as UUID
import qualified Hercules.API.Agent.Build.BuildTask as BuildTask
import qualified Hercules.API.Agent.Evaluate.EvaluateTask as EvaluateTask
import qualified Hercules.API.Agent.LifeCycle as LifeCycle
import qualified Hercules.API.Agent.LifeCycle.StartInfo as StartInfo
import qualified Hercules.API.Agent.Socket.AgentPayload as AgentPayload
import qualified Hercules.API.Agent.Socket.ServicePayload as ServicePayload
import Hercules.API.Agent.Tasks
  ( tasksSetStatus,
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
import Hercules.Agent.Socket as Socket
import Hercules.Agent.Socket (serviceChan)
import Hercules.Agent.Token (withAgentToken)
import Hercules.Error
  ( cap,
    exponential,
    retry,
  )
import Protolude hiding
  ( bracket,
    forkFinally,
    handle,
    killThread,
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
  case Options.mode opts of
    Options.Run -> run env cfg
    Options.Test -> testConfiguration env cfg

testConfiguration :: Env.Env -> Config.FinalConfig -> IO ()
testConfiguration _env _cfg = do
  -- Room for checks that are not in Init.newEnv
  pass

run :: Env.Env -> Config.FinalConfig -> IO ()
run env _cfg = do
  tasks <- newIORef mempty
  Env.runApp env
    $ katipAddContext (sl "agent-version" (A.String herculesAgentVersion))
    $ withAgentToken
    $ withLifeCycle \hello ->
      withReliableSocket hello tasks \socket ->
        withApplicationLevelPinger socket $ do
          logLocM InfoS "Agent online."
          forever $ do
            (liftIO $ atomically $ readTChan $ serviceChan socket) >>= \case
              ServicePayload.ServiceInfo _ -> pass
              ServicePayload.StartEvaluation evalTask ->
                launchTask tasks (Task.upcastId $ EvaluateTask.id evalTask) do
                  Cachix.withCaches do
                    Evaluate.performEvaluation evalTask
                    pure $ TaskStatus.Successful ()
              ServicePayload.StartBuild buildTask ->
                launchTask tasks (Task.upcastId $ BuildTask.id buildTask) do
                  Cachix.withCaches $
                    Build.performBuild buildTask
              ServicePayload.Cancel cancellation -> cancelTask tasks cancellation

launchTask :: IORef (Map (Id (Task Task.Any)) ThreadId) -> Id (Task Task.Any) -> App TaskStatus.TaskStatus -> App ()
launchTask tasks taskId doWork = withNamedContext "task" taskId do
  let insertSelf = do
        me <- liftIO myThreadId
        join $ atomicModifyIORef tasks \tasks0 ->
          case M.lookup taskId tasks0 of
            Just preexist ->
              ( tasks0,
                withNamedContext "preexistingThread" (show preexist :: Text) do
                  logLocM ErrorS "Refusing to overwrite thread for task"
              )
            Nothing -> (M.insert taskId me tasks0, pass)
      done st = do
        report' st
        liftIO $ atomicModifyIORef tasks \tasks0 ->
          (M.delete taskId tasks0, ())
      report' (Right s@(TaskStatus.Terminated _)) = do
        logLocM InfoS "Completed failed task"
        report s
      report' (Right s@(TaskStatus.Successful _)) = do
        logLocM InfoS "Completed task successfully"
        report s
      report' (Right s@(TaskStatus.Exceptional e)) = withNamedContext "message" e do
        logLocM ErrorS "Exceptional status in task"
        report s
      report' (Left e) = withNamedContext "message" (displayException e) $
        withNamedContext "exception" (show e :: Text) do
          logLocM ErrorS "Exception in task"
          report $ TaskStatus.Exceptional $ toSL $ displayException e
      -- TODO use socket
      report status =
        retry (cap 60 exponential)
          $ noContent
          $ runHerculesClient
          $ tasksSetStatus tasksClient taskId status
  void @_ @ThreadId $
    flip forkFinally done do
      insertSelf
      logLocM InfoS "Starting task"
      doWork

cancelTask :: IORef (Map (Id (Task Task.Any)) ThreadId) -> ServicePayload.Cancel -> App ()
cancelTask tasks cancellation = do
  let taskId = ServicePayload.taskId cancellation
  withNamedContext "taskId" taskId do
    tasksNow <- readIORef tasks
    case M.lookup taskId tasksNow of
      Just x -> do
        logLocM DebugS "Killing thread for cancelled task"
        killThread x
      Nothing ->
        logLocM DebugS "cancelTask: No such task"

withLifeCycle :: (StartInfo.Hello -> App a) -> App a
withLifeCycle app = do
  agentInfo <- extractAgentInfo
  now <- liftIO getCurrentTime
  freshId <- Id <$> liftIO UUID.nextRandom
  let startInfo = StartInfo.StartInfo {id = freshId, startTime = now}
      hello = StartInfo.Hello
        { agentInfo = agentInfo,
          startInfo = startInfo,
          tasksInProgress = []
        }
      req r =
        retry (cap 60 exponential)
          $ noContent
          $ runHerculesClient
          $ r
      sayGoodbye = req $ LifeCycle.goodbye lifeCycleClient startInfo
  bracket pass (\() -> sayGoodbye) (\() -> app hello)

withApplicationLevelPinger :: Socket -> App a -> App a
withApplicationLevelPinger socket = fmap (either identity identity) . race pinger
  where
    pinger = forever $ do
      liftIO $ atomically $ Socket.write socket AgentPayload.Ping
      liftIO $ threadDelay (oneSecond * 30)
    oneSecond = 1000 * 1000
