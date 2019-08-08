{-# LANGUAGE DeriveAnyClass #-}
module Hercules.Agent.WorkerProtocol.Event where

import           Prelude                        ( )
import           Protolude
import           Data.Binary
import           Hercules.Agent.WorkerProtocol.Event.Attribute
import           Hercules.Agent.WorkerProtocol.Event.AttributeError

data Event = Attribute Attribute
           | AttributeError AttributeError
           | EvaluationDone
           | Error Text
           | Build Text
             deriving (Generic, Binary, Show, Eq)
