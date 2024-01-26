{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.WorkerProtocol.Command.Effect where

import Data.Aeson qualified as A
import Data.Binary
import Hercules.API.Id (Id)
import Hercules.Agent.Sensitive
import Hercules.Agent.WorkerProtocol.Orphans ()
import Hercules.Agent.WorkerProtocol.ViaJSON (ViaJSON)
import Hercules.Formats.Mountable (Mountable)
import Hercules.Secrets (SecretContext)
import Protolude

data Effect = Effect
  { drvPath :: Text,
    apiBaseURL :: Text,
    inputDerivationOutputPaths :: [ByteString],
    secretsPath :: FilePath,
    serverSecrets :: Sensitive (ViaJSON (Map Text (Map Text A.Value))),
    token :: Sensitive Text,
    projectId :: Id "project",
    projectPath :: Text,
    secretContext :: SecretContext,
    configuredMountables :: Sensitive (ViaJSON (Map Text (Mountable)))
  }
  deriving (Generic, Binary, Show, Eq)
