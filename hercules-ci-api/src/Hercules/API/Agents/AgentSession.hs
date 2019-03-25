{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Agents.AgentSession where

import           Hercules.API.Prelude

import           Hercules.API.Agents.ClusterJoinToken ( ClusterJoinToken )

data AgentSession = AgentSession
  { hostname :: Text
  , clusterJoinTokenId :: Id ClusterJoinToken
  , id :: Id AgentSession
  , description :: Text
  , agentVersion :: Text
  , nixVersion :: Text
  , architectures :: [Text]
  , creation :: UTCTime
  , lastSeen :: Maybe UTCTime
  , available :: Bool
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
