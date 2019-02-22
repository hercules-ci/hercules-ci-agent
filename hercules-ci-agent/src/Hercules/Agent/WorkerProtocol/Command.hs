{-# LANGUAGE DeriveAnyClass #-}
module Hercules.Agent.WorkerProtocol.Command where

import           Protolude
import           Data.Binary
import qualified Hercules.Agent.WorkerProtocol.Command.Eval
                                               as Eval

data Command = Eval Eval.Eval
             | Build Void -- TODO: add fields
             deriving (Generic, Binary, Show, Eq)
