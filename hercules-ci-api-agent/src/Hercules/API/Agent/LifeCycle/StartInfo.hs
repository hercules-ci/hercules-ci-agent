{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.LifeCycle.StartInfo where

import Hercules.API.Agent.LifeCycle.AgentInfo (AgentInfo)
import Hercules.API.Prelude

data Hello
  = Hello
      { agentInfo :: AgentInfo,
        startInfo :: StartInfo
      }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data StartInfo
  = StartInfo
      { id :: Id StartInfo,
        startTime :: UTCTime
      }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
