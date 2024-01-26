{-# LANGUAGE DeriveAnyClass #-}

module AgentTask where

import Hercules.API.Agent.Build.BuildTask
  ( BuildTask,
  )
import Hercules.API.Agent.Effect.EffectTask (EffectTask)
import Hercules.API.Agent.Evaluate.EvaluateTask
  ( EvaluateTask,
  )
import Hercules.API.Prelude

data AgentTask
  = Evaluate EvaluateTask
  | Build BuildTask
  | Effect EffectTask
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
