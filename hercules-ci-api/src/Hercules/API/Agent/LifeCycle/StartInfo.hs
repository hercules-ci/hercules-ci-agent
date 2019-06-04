{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Agent.LifeCycle.StartInfo where

import Hercules.API.Prelude

import Hercules.API.Agents.AgentInfo(AgentInfo)

data Hello = Hello
  { agentInfo :: AgentInfo
  , startInfo :: StartInfo
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data StartInfo = StartInfo
  { id :: Id StartInfo
  , startTime :: UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
