{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.AgentTask where

import           Hercules.API.Prelude
import qualified Hercules.API.EvaluateTask     as EvaluateTask

data AgentTask = Evaluate EvaluateTask.EvaluateTask
               | Build Text
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
