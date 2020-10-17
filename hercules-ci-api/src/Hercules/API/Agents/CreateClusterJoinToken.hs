{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agents.CreateClusterJoinToken where

import Hercules.API.Prelude

-- The owner account that the token applies to is in the path.
data CreateClusterJoinToken
  = CreateClusterJoinToken
      { description :: Text
      }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
