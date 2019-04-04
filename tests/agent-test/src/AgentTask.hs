{-# LANGUAGE DeriveAnyClass #-}
module AgentTask where

import           Hercules.API.Prelude
import qualified Hercules.API.Agent.Evaluate.EvaluateTask
                                               as EvaluateTask

data AgentTask = Evaluate EvaluateTask.EvaluateTask
               | Build Text
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
