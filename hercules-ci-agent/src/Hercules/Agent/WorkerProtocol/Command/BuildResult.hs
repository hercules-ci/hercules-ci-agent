{-# LANGUAGE DeriveAnyClass #-}
module Hercules.Agent.WorkerProtocol.Command.BuildResult where

import           Protolude
import           Data.Binary

data BuildResult = BuildResult Text BuildStatus
  deriving (Generic, Binary, Show, Eq)

data BuildStatus
  = Success
  | Failure
  | Exceptional Text
  deriving (Generic, Binary, Show, Eq)
