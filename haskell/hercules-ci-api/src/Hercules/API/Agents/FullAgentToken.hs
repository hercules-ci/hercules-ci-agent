{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Agents.FullAgentToken where

import           Hercules.API.Prelude

import           Hercules.API.Agents.AgentToken ( AgentToken )

data FullAgentToken = FullAgentToken
  { metadata :: AgentToken
  , token :: Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
