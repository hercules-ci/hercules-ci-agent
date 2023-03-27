{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}

module Hercules.Agent
  ( main,
  )
where

import Control.Concurrent.Async.Lifted
  ( race,
    withAsync,
  )
import Control.Concurrent.Lifted (forkFinally, killThread)
import Control.Concurrent.STM.TChan
import Control.Exception.Lifted (bracket)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson qualified as A
import Data.Map qualified as M
import Data.Time (getCurrentTime)
import Data.UUID.V4 qualified as UUID
import Hercules.API.Agent.Build.BuildTask qualified as BuildTask
import Hercules.API.Agent.Effect.EffectTask qualified as EffectTask
import Hercules.API.Agent.Evaluate.EvaluateTask qualified as EvaluateTask
import Hercules.API.Agent.LifeCycle qualified as LifeCycle
import Hercules.API.Agent.LifeCycle.StartInfo (tasksInProgress)
import Hercules.API.Agent.LifeCycle.StartInfo qualified as StartInfo
import Hercules.API.Agent.Socket.AgentPayload qualified as AgentPayload
import Hercules.API.Agent.Socket.ServicePayload qualified as ServicePayload
import Hercules.API.Agent.Tasks
  ( tasksSetStatus,
  )
import Hercules.API.Id (Id (Id))
import Hercules.API.Servant (noContent)
import Hercules.API.Task (Task)
import Hercules.API.Task qualified as Task
import Hercules.API.TaskStatus qualified as TaskStatus
import Hercules.Agent.AgentSocket (withAgentSocket)
import Hercules.Agent.Build qualified as Build
import Hercules.Agent.CabalInfo (herculesAgentVersion)
import Hercules.Agent.Cache qualified as Cache
#if ! MIN_VERSION_cachix(1, 4, 0) || MIN_VERSION_cachix(1, 5, 0)
import Hercules.Agent.Cachix.Env qualified as Cachix.Env
#endif
import Hercules.Agent.Client
  ( lifeCycleClient,
    tasksClient,
  )
import Hercules.Agent.Config qualified as Config
import Hercules.Agent.Effect qualified as Effect
import Hercules.Agent.Env
  ( App,
    runHerculesClient,
  )
import Hercules.Agent.Env qualified as Env
import Hercules.Agent.EnvironmentInfo (extractAgentInfo)
import Hercules.Agent.Evaluate qualified as Evaluate
import Hercules.Agent.Init qualified as Init
import Hercules.Agent.Log
import Hercules.Agent.LogSocket (LogSettings (LogSettings), withLoggerNoFlush)
import Hercules.Agent.LogSocket qualified
import Hercules.Agent.Netrc qualified as Netrc
import Hercules.Agent.Options qualified as Options
import Hercules.Agent.STM
import Hercules.Agent.ServiceInfo qualified
import Hercules.Agent.Socket qualified as Socket
import Hercules.Agent.Token (withAgentToken)
import Hercules.CNix.Store qualified as CNix.Store
import Hercules.Error
  ( cap,
    exponential,
    retry,
  )
import Network.URI (uriToString)
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
import Prelude qualified

main :: IO ()
main = do
  opts <- Options.parse
  let cfgPath = Options.configFile opts
  cfg <- Config.finalizeConfig cfgPath =<< Config.readConfig cfgPath
  Init.initCNix cfg
  Init.setupLogging cfg $ \logEnv -> do
    Init.withEnv cfg logEnv $ \env -> do
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
    logLocM WarningS "Your config does not indicate you have set up the user that runs the agent as a trusted-user on the system. Running the agent as a trusted-user ensures that your cache configuration is compatible with the system and it improves performance. The NixOS and nix-darwin modules configure this automatically. If this agent was set up with a manually written config file, add the user that runs the agent to nix.settings.trusted-users in your system configuration, or add it to the trusted-users line in your system nix.conf."

run :: Env.Env -> Config.FinalConfig -> IO ()
run env _cfg = do
  Env.runApp env $
    katipAddContext (sl "agent-version" (A.String herculesAgentVersion)) $
      (configureLimits >>) $
        (configChecks >>) $
          withAgentToken $
            withLifeCycle \hello -> withTaskState \tasks ->
              withAgentSocket hello tasks \socket ->
                withApplicationLevelPinger socket $ do
                  logLocM InfoS "Agent online."
                  storeProtocolVersion <- liftIO do
                    CNix.Store.openStore >>= CNix.Store.getStoreProtocolVersion

                  let baseURL = env.serviceInfo.bulkSocketBaseURL & toString
                      toString uri = uriToString identity uri "" & toS

                  let launchTask' task path_ f = launchTask tasks socket (Task.upcastId task.id) do
                        Netrc.withNixNetrc $ Cache.withCaches do
                          withLoggerNoFlush
                            ("task" <> show task.id)
                            storeProtocolVersion
                            LogSettings
                              { path = path_,
                                baseURL = baseURL,
                                token = task.logToken
                              }
                            \logger -> do
                              f logger

                  forever $ do
                    liftIO (atomically $ readTChan $ socket.serviceChan) >>= \case
                      ServicePayload.ServiceInfo _ -> pass
                      ServicePayload.StartEvaluation evalTask ->
                        launchTask' evalTask "/api/v1/logs/build/socket" \logger -> do
                          Netrc.withNixNetrc $ Cache.withCaches do
                            CNix.Store.withStore \store -> do
                              Evaluate.performEvaluation logger store evalTask
                            pure $ TaskStatus.Successful ()
                      ServicePayload.StartBuild buildTask ->
                        launchTask' buildTask "/api/v1/logs/build/socket" \logger -> do
                          Build.performBuild logger buildTask
                      ServicePayload.StartEffect effectTask ->
                        launchTask' effectTask "/api/v1/logs/build/socket" \logger -> do
                          Effect.performEffect logger effectTask
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
          case fromException e of
            Just (FatalError msg) ->
              report $ TaskStatus.Exceptional msg
            _ ->
              report $ TaskStatus.Exceptional $ toS $ displayException e
      -- TODO use socket
      report status =
        retry (cap 60 exponential) $
          noContent $
            runHerculesClient $
              tasksSetStatus tasksClient taskId status
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
      hello =
        StartInfo.Hello
          { agentInfo = agentInfo,
            startInfo = startInfo,
            tasksInProgress = []
          }
      req r =
        retry (cap 60 exponential) $
          noContent $
            runHerculesClient r
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
  let lims' =
        ResourceLimits
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
      showLimit ResourceLimitInfinity = "infinity"
      showLimit ResourceLimitUnknown = A.Null
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
