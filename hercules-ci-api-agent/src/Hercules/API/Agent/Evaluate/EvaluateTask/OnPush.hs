{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.EvaluateTask.OnPush where

import Hercules.API.Agent.Evaluate.ImmutableInput (ImmutableInput)
import Hercules.API.Prelude

data OnPush = MkOnPush
  { name :: Text,
    -- | extraInputs
    inputs :: Map Text ImmutableInput
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)
