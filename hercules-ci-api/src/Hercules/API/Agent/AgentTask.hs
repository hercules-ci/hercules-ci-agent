{-# LANGUAGE DeriveAnyClass #-}

module AgentTask where

import qualified Hercules.API.Agent.Evaluate.EvaluateTask as EvaluateTask
import Hercules.API.Prelude

data AgentTask
  = Evaluate EvaluateTask.EvaluateTask
  | Build Text
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
