{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Socket.AgentPayload where

import Hercules.API.Agent.LifeCycle.StartInfo (Hello)
import Hercules.API.Prelude

data AgentPayload
  = Hello Hello
  | Ping
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
