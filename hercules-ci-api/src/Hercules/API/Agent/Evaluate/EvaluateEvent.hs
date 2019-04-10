{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Agent.Evaluate.EvaluateEvent where

import           Hercules.API.Prelude
import           Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEvent
                                                ( AttributeEvent )
import           Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeErrorEvent
                                                ( AttributeErrorEvent )
import           Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo
                                                ( DerivationInfo )
import           Hercules.API.Message           ( Message )

data EvaluateEvent
  = Attribute AttributeEvent
  | AttributeError AttributeErrorEvent
  | Message Message
  | DerivationInfo DerivationInfo
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
