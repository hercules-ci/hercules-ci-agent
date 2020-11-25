{-# LANGUAGE BlockArguments #-}

module Hercules.Agent
  ( main,
  )
where

import Control.Concurrent.Async.Lifted
  ( race,
    withAsync,
  )
import Control.Concurrent.Lifted (forkFinally, killThread)
import Control.Concurrent.STM (TVar, readTVar)
import Control.Concurrent.STM.TChan
import Control.Exception (displayException)
import Control.Exception.Lifted (bracket)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Data.Aeson as A
import qualified Data.Map as M
import Data.Time (getCurrentTime)
import qualified Data.UUID.V4 as UUID
import qualified Hercules.API.Agent.Build.BuildTask as BuildTask
import qualified Hercules.API.Agent.Evaluate.EvaluateTask as EvaluateTask
import qualified Hercules.API.Agent.LifeCycle as LifeCycle
import qualified Hercules.API.Agent.LifeCycle.StartInfo as StartInfo
import Hercules.API.Agent.LifeCycle.StartInfo (tasksInProgress)
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
import Hercules.Agent.AgentSocket (withAgentSocket)
import qualified Hercules.Agent.Build as Build
import Hercules.Agent.CabalInfo (herculesAgentVersion)
import qualified Hercules.Agent.Cache as Cache
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
import Hercules.Agent.STM
import Hercules.Agent.Socket as Socket
import Hercules.Agent.Socket (serviceChan)
import Hercules.Agent.Token (withAgentToken)
import Hercules.Error
  ( cap,
    exponential,
    retry,
  )
import Protolude hiding
  ( atomically,
    bracket,
    catch,
    forkFinally,
    handle,
    killThread,
    race,
    retry,
    withAsync,
    withMVar,
  )
import System.Posix.Resource
import UnliftIO.Exception (catch)
import qualified Prelude

main :: IO ()
main = do
  Init.initCNix
  opts <- Options.parse
  let cfgPath = Options.configFile opts
  cfg <- Config.finalizeConfig cfgPath =<< Config.readConfig cfgPath
  Init.setupLogging cfg $ \logEnv -> do
    env <- Init.newEnv cfg logEnv
    case Options.mode opts of
      Options.Run -> run env cfg
      Options.Test -> testConfiguration env cfg

testConfiguration :: Env.Env -> Config.FinalConfig -> IO ()
testConfiguration _env _cfg = do
  -- Room for checks that are not in Init.newEnv
  pass

configChecks :: App ()
configChecks = do
  trusted <- asks (Config.nixUserIsTrusted . Env.config)
  when (not trusted) do
    logLocM WarningS "Your config does not indicate you have set up your user as a trusted user on the system. Running the agent as a trusted user ensures that your cache configuration is compatible with the system and improves performance if you have more than one agent. The NixOS and nix-darwin modules should configure this automatically. If this agent was set up with a manually written config file, see https://docs.hercules-ci.com/hercules-ci/reference/agent-config/"

run :: Env.Env -> Config.FinalConfig -> IO ()
run env _cfg = do
  Env.runApp env
    $ katipAddContext (sl "agent-version" (A.String herculesAgentVersion))
    $ (configureLimits >>)
    $ (configChecks >>)
    $ withAgentToken
    $ withLifeCycle \hello -> withTaskState \tasks ->
      withAgentSocket hello tasks \socket ->
        withApplicationLevelPinger socket $ do
          logLocM InfoS "Agent online."
          forever $ do
            (liftIO $ atomically $ readTChan $ serviceChan socket) >>= \case
              ServicePayload.ServiceInfo _ -> pass
              ServicePayload.StartEvaluation evalTask ->
                launchTask tasks socket (Task.upcastId $ EvaluateTask.id evalTask) do
                  Cache.withCaches do
                    Evaluate.performEvaluation evalTask
                    pure $ TaskStatus.Successful ()
              ServicePayload.StartBuild buildTask ->
                launchTask tasks socket (Task.upcastId $ BuildTask.id buildTask) do
                  Cache.withCaches $
                    Build.performBuild buildTask
              ServicePayload.Cancel cancellation -> cancelTask tasks socket cancellation

withTaskState :: (TVar (Map (Id (Task Task.Any)) ThreadId) -> App a) -> App a
withTaskState f = do
  tasks <- newTVarIO mempty
  withAsync (taskStateMonitor tasks) \_ ->
    f tasks

taskStateMonitor :: TVar (Map (Id (Task Task.Any)) ThreadId) -> App ()
taskStateMonitor tasks = watchChange mempty
  where
    watchChange :: Map (Id (Task Task.Any)) ThreadId -> App ()
    watchChange current = do
      new <- atomically do
        now <- readTVar tasks
        guard $ now /= current
        pure now
      katipAddContext (sl "state" (A.toJSON $ fmap Prelude.show new)) do
        logLocM DebugS "Task state updated"
      watchChange new

launchTask :: TVar (Map (Id (Task Task.Any)) ThreadId) -> Env.AgentSocket -> Id (Task Task.Any) -> App TaskStatus.TaskStatus -> App ()
launchTask tasks socket taskId doWork = withNamedContext "task" taskId do
  let insertSelf = do
        me <- liftIO myThreadId
        join $ modifyTVarIO tasks \tasks0 ->
          case M.lookup taskId tasks0 of
            Just preexist ->
              ( tasks0,
                withNamedContext "preexistingThread" (show preexist :: Text) do
                  logLocM ErrorS "Refusing to overwrite thread for task"
              )
            Nothing -> (M.insert taskId me tasks0, pass)
      done st = do
        report' st
        modifyTVarIO tasks \tasks0 ->
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
          report $ TaskStatus.Exceptional $ toS $ displayException e
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
      liftIO $ atomically $ Socket.write socket $ AgentPayload.Started $ AgentPayload.MkStarted {taskId = taskId}
      doWork

cancelTask :: TVar (Map (Id (Task Task.Any)) ThreadId) -> Env.AgentSocket -> ServicePayload.Cancel -> App ()
cancelTask tasks socket cancellation = do
  let taskId = ServicePayload.taskId cancellation
  withNamedContext "taskId" taskId do
    tasksNow <- readTVarIO tasks
    case M.lookup taskId tasksNow of
      Just x -> do
        logLocM DebugS "Killing thread for cancelled task"
        killThread x
        atomically $ Socket.write socket $ AgentPayload.Cancelled $ AgentPayload.MkCancelled {taskId = taskId}
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

withApplicationLevelPinger :: Env.AgentSocket -> App a -> App a
withApplicationLevelPinger socket = fmap (either identity identity) . race pinger
  where
    pinger = forever $ do
      liftIO $ atomically $ Socket.write socket AgentPayload.Ping
      liftIO $ threadDelay (oneSecond * 30)
    oneSecond = 1000 * 1000

configureLimits :: (MonadUnliftIO m, KatipContext m) => m ()
configureLimits = do
  tryIncreaseResourceLimitTo ResourceOpenFiles "open files" (ResourceLimit 65536)

tryIncreaseResourceLimitTo :: (MonadUnliftIO m, KatipContext m) => Resource -> Text -> ResourceLimit -> m ()
tryIncreaseResourceLimitTo resource resourceName target = katipAddContext (sl "resource" resourceName) do
  lims <- liftIO $ getResourceLimit resource
  let lims' = ResourceLimits
        { softLimit = target & atLeast (softLimit lims) & atMost (hardLimit lims),
          hardLimit = hardLimit lims
        }
      atLeast ResourceLimitInfinity _ = ResourceLimitInfinity
      atLeast _ ResourceLimitInfinity = ResourceLimitInfinity
      atLeast ResourceLimitUnknown r = r
      atLeast l ResourceLimitUnknown = l
      atLeast (ResourceLimit a) (ResourceLimit b) | a > b = ResourceLimit a
      atLeast _ r = r
      atMost (ResourceLimit a) (ResourceLimit b) | a > b = ResourceLimit b
      atMost (ResourceLimit a) _ = ResourceLimit a
      atMost ResourceLimitInfinity r = r
      atMost l ResourceLimitInfinity = l
      atMost ResourceLimitUnknown r = r
      -- atMost l ResourceLimitUnknown = l -- already covered
      showLimit (ResourceLimitInfinity) = "infinity"
      showLimit (ResourceLimitUnknown) = A.Null
      showLimit (ResourceLimit n) = A.Number $ fromIntegral n
  liftIO (setResourceLimit resource lims') `catch` \e ->
    katipAddContext
      ( sl "message" (displayException (e :: SomeException))
          <> sl "soft" (showLimit (softLimit lims'))
          <> sl "hard" (showLimit (hardLimit lims'))
      )
      do
        logLocM WarningS "Could not increase resource limit"
  limsFinal <- liftIO (getResourceLimit resource)
  katipAddContext
    ( sl "soft" (showLimit (softLimit limsFinal))
        <> sl "hard" (showLimit (hardLimit limsFinal))
    )
    do
      logLocM DebugS "Resource limit"
