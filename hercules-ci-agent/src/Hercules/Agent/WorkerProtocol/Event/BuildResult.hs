{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Event.BuildResult where

import Data.Binary
import Hercules.Agent.WorkerProtocol.OutputInfo (OutputInfo)
import Protolude

data BuildResult
  = BuildFailure {errorMessage :: Text}
  | BuildSuccess {outputs :: [OutputInfo]}
  deriving (Generic, Binary, Show, Eq)
