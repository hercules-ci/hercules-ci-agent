{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Command where

import Data.Binary
import Hercules.Agent.WorkerProtocol.Command.Build qualified as Build
import Hercules.Agent.WorkerProtocol.Command.BuildResult qualified as BuildResult
import Hercules.Agent.WorkerProtocol.Command.Effect qualified as Effect
import Hercules.Agent.WorkerProtocol.Command.Eval qualified as Eval
import Protolude

-- | Information flowing from the environment to the worker. The name isn't
-- accurate as it includes other information than just commands,
-- e.g. 'BuildResult'.
data Command
  = Eval Eval.Eval
  | BuildResult BuildResult.BuildResult
  | Build Build.Build
  | Effect Effect.Effect
  deriving (Generic, Binary, Show, Eq)
