{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Event.DaemonStarted where

import Data.Binary
import Protolude

data DaemonStarted = DaemonStarted
  {_dummy :: Bool {- make its binary representation non-empty -}}
  deriving (Generic, Binary, Show, Eq)
