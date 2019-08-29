{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Command where

import Data.Binary
import qualified Hercules.Agent.WorkerProtocol.Command.BuildResult as BuildResult
import qualified Hercules.Agent.WorkerProtocol.Command.Eval as Eval
import Protolude

data Command
  = Eval Eval.Eval
  | BuildResult BuildResult.BuildResult
  deriving (Generic, Binary, Show, Eq)
