module Hercules.Agent
  ( main
  )
where

import           Protolude               hiding ( handle
                                                , withMVar
                                                , retry
                                                , race
                                                , bracket
                                                )

import           Control.Concurrent.Async.Lifted
                                                ( replicateConcurrently_, race )
import           Control.Concurrent.MVar.Lifted ( withMVar )
import           Control.Exception.Lifted       ( bracket )
import           Data.Time                      ( getCurrentTime )
import qualified Data.UUID.V4                 as UUID
import           Hercules.API                   ( noContent )
import           Hercules.API.Id                ( Id(Id) )
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
import qualified Hercules.API.Agent.LifeCycle
                                               as LifeCycle
import qualified Hercules.API.Agent.LifeCycle.StartInfo
                                               as StartInfo
import           Hercules.Agent.CabalInfo       ( herculesAgentVersion )
import qualified Hercules.Agent.Cachix         as Cachix
import           Hercules.Agent.Client          ( tasksClient
                                                , lifeCycleClient
                                                )
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
import           Hercules.Agent.EnvironmentInfo ( extractAgentInfo )

import           Hercules.Agent.Log
import qualified Data.Aeson                    as A

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
    $ withHerculesScribe
    $ withLifeCycle
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

withLifeCycle :: App a -> App a
withLifeCycle app = do
  agentInfo <- extractAgentInfo
  now <- liftIO getCurrentTime
  freshId <- Id <$> liftIO UUID.nextRandom

  let
    startInfo = StartInfo.StartInfo { id = freshId, startTime = now }

    hello = StartInfo.Hello
                { agentInfo = agentInfo
                , startInfo = startInfo
                }

    req r = retry (cap 60 exponential)
          $ noContent
          $ runHerculesClient
          $ r

    sayHello = req $ LifeCycle.hello lifeCycleClient hello

    pinger = forever $ do
      req $ LifeCycle.heartbeat lifeCycleClient startInfo

      let oneSecond = 1000 * 1000
      liftIO $ threadDelay (oneSecond * 60)

    sayGoodbye = req $ LifeCycle.goodbye lifeCycleClient startInfo

    withPinger = fmap (either identity identity)
                  . race pinger

  bracket sayHello (\() -> sayGoodbye) (\() -> withPinger app)


performTask :: Task Task.Any -> App ()
performTask task = contextually $ do
  happy <- handleExceptions performTaskPerType
  when happy reportSuccess
 where

  contextually = withNamedContext "task" (Task.id task)
     . withNamedContext "task-type" (Task.typ task)
  
  handleExceptions m = safeLiftedHandle (\e -> do
          logLocM ErrorS $ "Exception in task: " <> show (e :: SomeException)
          retry (cap 60 exponential)
            $ noContent
            $ runHerculesClient
            $ tasksSetStatus tasksClient
                             (Task.id task)
                             (TaskStatus.Exceptional $ show e)
          pure False
    ) (m >> pure True)

  performTaskPerType = do
    logLocM InfoS "Starting task"

    case Task.typ task of
      "eval" -> do
        let evalTask :: Task EvaluateTask.EvaluateTask
            evalTask = Task.uncheckedCast task
        Cachix.withCaches $
          Evaluate.performEvaluation evalTask
      "build" -> do
        let buildTask :: Task BuildTask.BuildTask
            buildTask = Task.uncheckedCast task
        Cachix.withCaches $
          Build.performBuild buildTask
      _ -> panicWithLog "Unknown task type"

  reportSuccess = do
    logLocM InfoS "Completed task successfully"

    retry (cap 60 exponential)
      $ noContent
      $ runHerculesClient
      $ tasksSetStatus tasksClient (Task.id task) (TaskStatus.Successful ())
