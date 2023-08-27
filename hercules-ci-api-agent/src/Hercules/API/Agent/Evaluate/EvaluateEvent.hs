{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.EvaluateEvent where

import Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEffectEvent (AttributeEffectEvent)
import Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeErrorEvent (AttributeErrorEvent)
import Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEvent (AttributeEvent)
import Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeIFDEvent (AttributeIFDEvent)
import Hercules.API.Agent.Evaluate.EvaluateEvent.BuildRequest (BuildRequest)
import Hercules.API.Agent.Evaluate.EvaluateEvent.BuildRequired (BuildRequired)
import Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo (DerivationInfo)
import Hercules.API.Agent.Evaluate.EvaluateEvent.JobConfig (JobConfig)
import Hercules.API.Agent.Evaluate.EvaluateEvent.Message (Message)
import Hercules.API.Agent.Evaluate.EvaluateEvent.OnPushHandlerEvent (OnPushHandlerEvent)
import Hercules.API.Agent.Evaluate.EvaluateEvent.OnScheduleHandlerEvent (OnScheduleHandlerEvent)
import Hercules.API.Agent.Evaluate.EvaluateEvent.PushedAll (PushedAll)
import Hercules.API.Agent.Evaluate.EvaluateEvent.SubstitutionQueryResult (SubstitutionQueryResult)
import Hercules.API.Prelude

data EvaluateEvent
  = Attribute AttributeEvent
  | AttributeEffect AttributeEffectEvent
  | AttributeError AttributeErrorEvent
  | AttributeIFD AttributeIFDEvent
  | Message Message
  | DerivationInfo DerivationInfo
  | PushedAll PushedAll
  | BuildRequired BuildRequired
  | SubstitutionQueryResult SubstitutionQueryResult
  | BuildRequest BuildRequest
  | JobConfig JobConfig
  | OnPushHandlerEvent OnPushHandlerEvent
  | OnScheduleHandlerEvent OnScheduleHandlerEvent
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)
