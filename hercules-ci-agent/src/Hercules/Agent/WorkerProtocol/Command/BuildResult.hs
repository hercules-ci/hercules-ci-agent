{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Command.BuildResult where

import Data.Binary
import Data.UUID (UUID)
import Protolude

data BuildResult = BuildResult Text UUID BuildStatus
  deriving (Generic, Binary, Show, Eq)

-- | Subset of @DerivationStatus@, with a @Binary@ instance.
data BuildStatus
  = Success
  | Failure
  | DependencyFailure
  deriving (Generic, Binary, Show, Eq)
