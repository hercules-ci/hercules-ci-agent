{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Agents.Agent where

import           Hercules.API.Prelude

import           Hercules.API.Agents.AgentToken ( AgentToken )

data Agent = Agent
  { id :: Id Agent
  , tokenId :: Id AgentToken
  , description :: Text
  , hostname :: Text
  , agentVersion :: Text
  , nixVersion :: Text
  , architectures :: [Text]
  , creation :: UTCTime
  , lastSeen :: Maybe UTCTime
  , available :: Bool
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
