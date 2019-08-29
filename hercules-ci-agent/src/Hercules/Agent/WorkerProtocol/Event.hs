{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Event where

import Data.Binary
import Hercules.Agent.WorkerProtocol.Event.Attribute
import Hercules.Agent.WorkerProtocol.Event.AttributeError
import Protolude
import Prelude ()

data Event
  = Attribute Attribute
  | AttributeError AttributeError
  | EvaluationDone
  | Error Text
  | Build Text Text
  deriving (Generic, Binary, Show, Eq)
