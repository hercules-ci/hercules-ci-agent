{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Agents.AgentSession where

import Data.OpenApi qualified as O3
import Hercules.API.Agents.ClusterJoinToken
  ( ClusterJoinToken,
  )
import Hercules.API.Labels
import Hercules.API.Prelude

data AgentSession = AgentSession
  { id :: Id AgentSession,
    clusterJoinTokenId :: Id ClusterJoinToken,
    description :: Text,
    hostname :: Text,
    agentVersion :: Text,
    nixVersion :: Text,
    platforms :: [Text],
    systemFeatures :: [Text],
    cachixPushCaches :: [Text],
    pushCaches :: [Text],
    substituters :: [Text],
    creation :: UTCTime,
    lastSeen :: Maybe UTCTime,
    revocation :: Maybe UTCTime,
    available :: Bool,
    concurrentTasks :: Int,
    labels :: Labels
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
