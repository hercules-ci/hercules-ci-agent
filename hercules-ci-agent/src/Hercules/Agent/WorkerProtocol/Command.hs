{-# LANGUAGE DeriveAnyClass #-}
module Hercules.Agent.WorkerProtocol.Command where

import           Protolude
import           Data.Binary
import qualified Hercules.Agent.WorkerProtocol.Command.Eval
                                               as Eval
import qualified Hercules.Agent.WorkerProtocol.Command.BuildResult
                                               as BuildResult

data Command
  = Eval Eval.Eval
  | BuildResult BuildResult.BuildResult
  deriving (Generic, Binary, Show, Eq)
