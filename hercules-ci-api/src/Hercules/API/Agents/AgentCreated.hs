{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Agents.AgentCreated where

import           Hercules.API.Prelude
import           Hercules.API.Agents.Agent      ( Agent )
import           Hercules.API.Agents.AgentToken ( AgentToken )

data AgentCreated = AgentCreated
  { id :: Id Agent
  , tokenId :: Id AgentToken
  , description :: Text
  , hostname :: Text
  , agentVersion :: Text
  , nixVersion :: Text
  , architectures :: [Text]
  , creation :: UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
