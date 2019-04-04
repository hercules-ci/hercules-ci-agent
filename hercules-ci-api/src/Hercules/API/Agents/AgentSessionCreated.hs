{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Agents.AgentSessionCreated where

import           Hercules.API.Prelude
import           Hercules.API.Agents.AgentSession
                                                ( AgentSession )
import           Hercules.API.Agents.ClusterJoinToken
                                                ( ClusterJoinToken )

data AgentSessionCreated = AgentSessionCreated
  { hostname :: Text
  , id :: Id AgentSession
  , tokenId :: Id ClusterJoinToken
  , description :: Text
  , agentVersion :: Text
  , nixVersion :: Text
  , architectures :: [Text]
  , creation :: UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
