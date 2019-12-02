{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

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
import Servant.Server.Generic

-- Provide uninitialised API records without warnings
dummyBuildEndpoints :: BuildAPI auth AsServer
dummyBuildEndpoints = BuildAPI {}

dummyEvalEndpoints :: EvalAPI auth AsServer
dummyEvalEndpoints = EvalAPI {}

dummyTasksEndpoints :: TasksAPI auth AsServer
dummyTasksEndpoints = TasksAPI {}

dummyLifeCycleEndpoints :: LifeCycleAPI auth AsServer
dummyLifeCycleEndpoints = LifeCycleAPI {}
