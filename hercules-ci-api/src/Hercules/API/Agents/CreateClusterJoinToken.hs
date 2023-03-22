{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Agents.CreateClusterJoinToken where

import Hercules.API.Prelude

-- The owner account that the token applies to is in the path.
data CreateClusterJoinToken = CreateClusterJoinToken
  { description :: Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
