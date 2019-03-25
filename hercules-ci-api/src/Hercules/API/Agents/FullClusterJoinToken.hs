{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Agents.FullClusterJoinToken where

import           Hercules.API.Prelude

import           Hercules.API.Agents.ClusterJoinToken ( ClusterJoinToken )

data FullClusterJoinToken = FullClusterJoinToken
  { metadata :: ClusterJoinToken
  , token :: Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
