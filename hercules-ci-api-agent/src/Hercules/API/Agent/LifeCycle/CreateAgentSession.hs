{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.LifeCycle.CreateAgentSession where

import Hercules.API.Prelude

{-# DEPRECATED CreateAgentSession "Use Hercules.API.Agents.CreateAgentSession_V2.CreateAgentSession" #-}

data CreateAgentSession = CreateAgentSession
  { hostname :: Text,
    agentVersion :: Text,
    nixVersion :: Text,
    architectures :: [Text]
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)
