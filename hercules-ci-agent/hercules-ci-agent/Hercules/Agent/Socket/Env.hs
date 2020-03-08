module Hercules.Agent.Socket.Env where

import Control.Concurrent.STM.TChan (TChan)
import qualified Hercules.API.Agent.Socket.AgentPayload as AgentPayload
import qualified Hercules.API.Agent.Socket.ServicePayload as ServicePayload
import Protolude

data Socket
  = Socket
      { write :: AgentPayload.AgentPayload -> STM (),
        serviceChan :: TChan ServicePayload.ServicePayload
      }
