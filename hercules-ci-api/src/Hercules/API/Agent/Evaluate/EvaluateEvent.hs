{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.EvaluateEvent where

import Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeErrorEvent
  ( AttributeErrorEvent
    )
import Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEvent
  ( AttributeEvent
    )
import Hercules.API.Agent.Evaluate.EvaluateEvent.BuildRequest
  ( BuildRequest
    )
import Hercules.API.Agent.Evaluate.EvaluateEvent.BuildRequired
  ( BuildRequired
    )
import Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo
  ( DerivationInfo
    )
import Hercules.API.Agent.Evaluate.EvaluateEvent.PushedAll
  ( PushedAll
    )
import Hercules.API.Message (Message)
import Hercules.API.Prelude

data EvaluateEvent
  = Attribute AttributeEvent
  | AttributeError AttributeErrorEvent
  | Message Message
  | DerivationInfo DerivationInfo
  | PushedAll PushedAll
  | BuildRequired BuildRequired
  | BuildRequest BuildRequest
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
