{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.LifeCycle.CreateAgentSession_V2 where

import Hercules.API.Agent.LifeCycle.AgentInfo
import Hercules.API.Prelude

data CreateAgentSession
  = CreateAgentSession
      { agentInfo :: AgentInfo
      }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
