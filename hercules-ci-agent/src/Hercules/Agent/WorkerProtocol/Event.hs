{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Event where

import Data.Binary
import Data.UUID (UUID)
import Hercules.Agent.WorkerProtocol.Event.Attribute
import Hercules.Agent.WorkerProtocol.Event.AttributeError
import Hercules.Agent.WorkerProtocol.Event.BuildResult
import Protolude
import Prelude ()

data Event
  = Attribute Attribute
  | AttributeError AttributeError
  | EvaluationDone
  | Error Text
  | Build Text Text (Maybe UUID)
  | BuildResult BuildResult
  | Exception Text
  deriving (Generic, Binary, Show, Eq)
