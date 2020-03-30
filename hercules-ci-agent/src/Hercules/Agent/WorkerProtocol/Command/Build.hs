{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Command.Build where

import Data.Binary
import Hercules.Agent.WorkerProtocol.LogSettings
import Protolude

data Build
  = Build
      { drvPath :: Text,
        inputDerivationOutputPaths :: [ByteString],
        logSettings :: LogSettings
      }
  deriving (Generic, Binary, Show, Eq)
