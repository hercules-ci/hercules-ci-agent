{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module DummyApi
  ( dummyEvalEndpoints
  , dummyTasksEndpoints
  , dummyMetaEndpoints
  )
where

import           Servant.Server.Generic
import           Hercules.API.Agent.Evaluate
import           Hercules.API.Agent.Tasks
import           Hercules.API.Agent.Meta

-- Provide uninitialised API records without warnings

dummyEvalEndpoints :: EvalAPI auth AsServer
dummyEvalEndpoints = EvalAPI{}

dummyTasksEndpoints :: TasksAPI auth AsServer
dummyTasksEndpoints = TasksAPI{}

dummyMetaEndpoints :: MetaAPI auth AsServer
dummyMetaEndpoints = MetaAPI{}
