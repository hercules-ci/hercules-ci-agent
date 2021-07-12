{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Command.Effect where

import Data.Binary
import Hercules.API.Id (Id)
import Hercules.Agent.Sensitive
import Hercules.Agent.WorkerProtocol.LogSettings
import Hercules.Agent.WorkerProtocol.Orphans ()
import Protolude

data Effect = Effect
  { drvPath :: Text,
    apiBaseURL :: Text,
    logSettings :: LogSettings,
    inputDerivationOutputPaths :: [ByteString],
    materializeDerivation :: Bool,
    secretsPath :: FilePath,
    token :: Sensitive Text,
    projectId :: Id "project",
    projectPath :: Text
  }
  deriving (Generic, Binary, Show, Eq)
