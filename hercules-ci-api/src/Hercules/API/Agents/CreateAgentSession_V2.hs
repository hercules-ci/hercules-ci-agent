{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agents.CreateAgentSession_V2 where

import Hercules.API.Agents.AgentInfo
import Hercules.API.Prelude

data CreateAgentSession
  = CreateAgentSession
      { agentInfo :: AgentInfo
        }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
