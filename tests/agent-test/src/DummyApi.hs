{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module DummyApi
  ( dummyBuildEndpoints
  , dummyEvalEndpoints
  , dummyTasksEndpoints
  , dummyAgentsEndpoints
  )
where

import           Servant.Server.Generic
import           Hercules.API.Agent.Evaluate
import           Hercules.API.Agent.Build
import           Hercules.API.Agent.Tasks
import           Hercules.API.Agents

-- Provide uninitialised API records without warnings

dummyBuildEndpoints :: BuildAPI auth AsServer
dummyBuildEndpoints = BuildAPI{}

dummyEvalEndpoints :: EvalAPI auth AsServer
dummyEvalEndpoints = EvalAPI{}

dummyTasksEndpoints :: TasksAPI auth AsServer
dummyTasksEndpoints = TasksAPI{}

dummyAgentsEndpoints :: AgentsAPI auth AsServer
dummyAgentsEndpoints = AgentsAPI{}
