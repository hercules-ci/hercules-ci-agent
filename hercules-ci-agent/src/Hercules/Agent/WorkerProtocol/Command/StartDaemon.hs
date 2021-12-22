{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Command.StartDaemon where

import Data.Binary
import Protolude

data StartDaemon = StartDaemon
  { socketPath :: FilePath
  }
  deriving (Generic, Binary, Show, Eq)
