{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Command.Effect where

import Data.Binary
import Hercules.Agent.Sensitive
import Hercules.Agent.WorkerProtocol.LogSettings
import Protolude

data Effect = Effect
  { drvPath :: Text,
    apiBaseURL :: Text,
    logSettings :: LogSettings,
    inputDerivationOutputPaths :: [ByteString],
    materializeDerivation :: Bool,
    secretsPath :: FilePath,
    token :: Sensitive Text
  }
  deriving (Generic, Binary, Show, Eq)
