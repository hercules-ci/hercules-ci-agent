{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.EvaluateTask.OnSchedule where

import Hercules.API.Agent.Evaluate.ImmutableInput (ImmutableInput)
import Hercules.API.Prelude

data OnSchedule = MkOnSchedule
  { name :: Text,
    -- | extraInputs
    extraInputs :: Map Text ImmutableInput
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)
