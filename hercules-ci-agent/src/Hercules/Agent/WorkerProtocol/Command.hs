{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Command where

import Data.Binary
import qualified Hercules.Agent.WorkerProtocol.Command.Build as Build
import qualified Hercules.Agent.WorkerProtocol.Command.BuildResult as BuildResult
import qualified Hercules.Agent.WorkerProtocol.Command.Effect as Effect
import qualified Hercules.Agent.WorkerProtocol.Command.Eval as Eval
import Protolude

data Command
  = Eval Eval.Eval
  | BuildResult BuildResult.BuildResult
  | Build Build.Build
  | Effect Effect.Effect
  deriving (Generic, Binary, Show, Eq)
