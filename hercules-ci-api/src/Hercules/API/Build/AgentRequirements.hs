{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Build.AgentRequirements where

import Hercules.API.Prelude

data AgentRequirements
  = AgentRequirements
      { platform :: Text,
        requiredSystemFeatures :: [Text]
      }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
