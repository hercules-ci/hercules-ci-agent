{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Agents.FullClusterJoinToken where

import Data.OpenApi qualified as O3
import Hercules.API.Agents.ClusterJoinToken
  ( ClusterJoinToken,
  )
import Hercules.API.Prelude

data FullClusterJoinToken = FullClusterJoinToken
  { metadata :: ClusterJoinToken,
    token :: Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
