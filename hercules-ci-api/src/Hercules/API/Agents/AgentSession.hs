{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Agents.AgentSession where

import           Hercules.API.Prelude

import           Hercules.API.Agents.ClusterJoinToken
                                                ( ClusterJoinToken )

data AgentSession = AgentSession
  { id :: Id AgentSession

  , clusterJoinTokenId :: Id ClusterJoinToken
  , description :: Text

  , hostname :: Text
  , agentVersion :: Text
  , nixVersion :: Text
  , platforms :: [Text]
  , systemFeatures :: [Text]
  , cachixPushCaches :: [Text]
  , substituters :: [Text]

  , creation :: UTCTime
  , lastSeen :: Maybe UTCTime
  , available :: Bool
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
