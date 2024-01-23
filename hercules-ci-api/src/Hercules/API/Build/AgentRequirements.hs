{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Build.AgentRequirements where

import Data.OpenApi qualified as O3
import Hercules.API.Prelude

data AgentRequirements = AgentRequirements
  { platform :: Text,
    requiredSystemFeatures :: [Text]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
