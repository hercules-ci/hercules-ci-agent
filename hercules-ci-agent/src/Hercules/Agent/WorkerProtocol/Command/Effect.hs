{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Command.Effect where

import Data.Binary
import Hercules.Agent.WorkerProtocol.LogSettings
import Protolude

data Effect
  = Effect
      { drvPath :: Text,
        logSettings :: LogSettings,
        inputDerivationOutputPaths :: [ByteString],
        materializeDerivation :: Bool,
        secretsPath :: FilePath
      }
  deriving (Generic, Binary, Show, Eq)
