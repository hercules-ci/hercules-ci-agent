{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module DummyApi
  ( dummyEvalEndpoints
  , dummyTasksEndpoints
  , dummyAgentsEndpoints
  )
where

import           Servant.Server.Generic
import           Hercules.API
import           Hercules.API.Agents

-- Provide uninitialised API records without warnings

dummyEvalEndpoints :: EvalAPI auth AsServer
dummyEvalEndpoints = EvalAPI{}

dummyTasksEndpoints :: TasksAPI auth AsServer
dummyTasksEndpoints = TasksAPI{}

dummyAgentsEndpoints :: AgentsAPI auth AsServer
dummyAgentsEndpoints = AgentsAPI{}
