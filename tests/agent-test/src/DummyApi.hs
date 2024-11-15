{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Werror=missing-fields -Wno-deprecations #-}

module DummyApi
  ( dummyBuildEndpoints,
    dummyEvalEndpoints,
    dummyTasksEndpoints,
    dummyLifeCycleEndpoints,
  )
where

import Hercules.API.Agent.Build
import Hercules.API.Agent.Evaluate
import Hercules.API.Agent.LifeCycle
import Hercules.API.Agent.Tasks
import Hercules.API.Task qualified
import Protolude
import Servant.Auth.Server
import Servant.Server.Generic

-- Provide uninitialised API records without warnings
dummyBuildEndpoints :: BuildAPI auth AsServer
dummyBuildEndpoints =
  BuildAPI
    { getBuild = \_ -> panic "not implemented: getBuild",
      updateBuild = \_ -> panic "not implemented: updateBuild",
      writeBuildLog = \_ -> panic "not implemented: writeBuildLog"
    }

dummyEvalEndpoints :: EvalAPI auth AsServer
dummyEvalEndpoints =
  EvalAPI
    { tasksGetEvaluation = \_ -> panic "not implemented: tasksGetEvaluation",
      tasksUpdateEvaluation = \_ -> panic "not implemented: tasksUpdateEvaluation",
      getDerivationStatus = \_ -> panic "not implemented: getDerivationStatus",
      getDerivationStatus2 = \_ -> panic "not implemented: getDerivationStatus2"
    }

dummyTasksEndpoints :: (auth ~ Auth a b) => TasksAPI auth AsServer
dummyTasksEndpoints =
  TasksAPI
    { tasksReady =
        -- Doesn't work; something's too strict
        -- panic "not implemented: tasksReady",
        \_auth -> pure (Nothing :: Maybe (Hercules.API.Task.Task Hercules.API.Task.Any)),
      tasksSetStatus = \_ -> panic "not implemented: tasksSetStatus",
      postLog = \_ -> panic "not implemented: postLog"
    }

dummyLifeCycleEndpoints :: LifeCycleAPI auth AsServer
dummyLifeCycleEndpoints =
  LifeCycleAPI
    { agentSessionCreate = \_ -> panic "not implemented: agentSessionCreate",
      hello = \_ -> panic "not implemented: hello",
      heartbeat = \_ -> panic "not implemented: heartbeat",
      goodbye = \_ -> panic "not implemented: goodbye",
      getServiceInfo = panic "not implemented: getServiceInfo"
    }
