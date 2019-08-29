{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Command.BuildResult where

import Data.Binary
import Protolude

data BuildResult = BuildResult Text BuildStatus
  deriving (Generic, Binary, Show, Eq)

-- | Subset of @DerivationStatus@, with a @Binary@ instance.
data BuildStatus
  = Success
  | Failure
  | DependencyFailure
  deriving (Generic, Binary, Show, Eq)
