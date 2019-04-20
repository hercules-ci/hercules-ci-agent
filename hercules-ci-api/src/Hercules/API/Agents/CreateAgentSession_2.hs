{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Agents.CreateAgentSession_2 where

import           Hercules.API.Prelude
import           Hercules.API.Agents.AgentInfo

data CreateAgentSession = CreateAgentSession
  { agentInfo :: AgentInfo
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
