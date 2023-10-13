{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Command.Build where

import Data.Binary
import Protolude

data Build = Build
  { drvPath :: Text,
    inputDerivationOutputPaths :: [ByteString],
    materializeDerivation :: Bool,
    materializePlatforms :: [ByteString]
  }
  deriving (Generic, Binary, Show, Eq)
