{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ConstraintKinds #-}
module Hercules.API.Agent.Evaluate.EvaluateEvent.BuildRequired where

import           Hercules.API.Prelude

data BuildRequired = BuildRequired
  { derivationPath :: Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
