{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Hercules.API.Agent.Socket.AgentPayload where

import Hercules.API.Agent.LifeCycle.StartInfo (Hello)
import Hercules.API.Prelude
import Hercules.API.Task as Task

data AgentPayload
  = Hello Hello
  | Ping
  | Started Started
  | Cancelled Cancelled
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)

data Started = MkStarted {taskId :: Id (Task Task.Any)}
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)

data Cancelled = MkCancelled {taskId :: Id (Task Task.Any)}
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)
