{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Command.Effect where

import Data.Binary
import Hercules.API.Id (Id)
import Hercules.Agent.Sensitive
import Hercules.Agent.WorkerProtocol.LogSettings
import Hercules.Agent.WorkerProtocol.Orphans ()
import Hercules.Agent.WorkerProtocol.ViaJSON (ViaJSON)
import Hercules.Secrets (SecretContext)
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
    projectPath :: Text,
    secretContext :: SecretContext
  }
  deriving (Generic, Binary, Show, Eq)
