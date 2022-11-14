{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hercules.Agent.WorkerProtocol.Event where

import Data.Binary (Binary)
import Data.UUID (UUID)
import Hercules.API.Agent.Evaluate.EvaluateEvent.OnPushHandlerEvent (OnPushHandlerEvent)
import Hercules.API.Agent.Evaluate.EvaluateEvent.OnScheduleHandlerEvent (OnScheduleHandlerEvent)
import Hercules.Agent.WorkerProtocol.Event.Attribute (Attribute)
import Hercules.Agent.WorkerProtocol.Event.AttributeError (AttributeError)
import Hercules.Agent.WorkerProtocol.Event.AttributeIFD (AttributeIFD)
import Hercules.Agent.WorkerProtocol.Event.BuildResult (BuildResult)
import Hercules.Agent.WorkerProtocol.ViaJSON (ViaJSON)
import Protolude

data Event
  = Attribute Attribute
  | AttributeError AttributeError
  | AttributeIFD AttributeIFD
  | EvaluationDone
  | Error Text
  | Build ByteString Text (Maybe UUID) Bool
  | BuildResult BuildResult
  | EffectResult Int
  | JobConfig
  | OnPushHandler (ViaJSON OnPushHandlerEvent)
  | OnScheduleHandler (ViaJSON OnScheduleHandlerEvent)
  | Exception Text
  deriving (Generic, Binary, Show, Eq)
