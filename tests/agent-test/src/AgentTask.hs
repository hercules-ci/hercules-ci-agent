{-# LANGUAGE DeriveAnyClass #-}
module AgentTask where

import           Hercules.API.Prelude
import           Hercules.API.Agent.Evaluate.EvaluateTask
                                               ( EvaluateTask)
import           Hercules.API.Agent.Build.BuildTask
                                               ( BuildTask)

data AgentTask = Evaluate EvaluateTask
               | Build BuildTask
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
