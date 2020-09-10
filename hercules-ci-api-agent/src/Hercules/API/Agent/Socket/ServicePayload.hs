{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Socket.ServicePayload where

import Hercules.API.Agent.Build.BuildTask (BuildTask)
import Hercules.API.Agent.Effect.EffectTask (EffectTask)
import Hercules.API.Agent.Evaluate.EvaluateTask (EvaluateTask)
import Hercules.API.Agent.LifeCycle.ServiceInfo (ServiceInfo)
import Hercules.API.Prelude
import Hercules.API.Task

data Cancel = MkCancel {taskId :: Id (Task Any)}
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)

data ServicePayload
  = ServiceInfo ServiceInfo
  | StartEvaluation EvaluateTask
  | StartBuild BuildTask
  | StartEffect EffectTask
  | Cancel Cancel
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)
