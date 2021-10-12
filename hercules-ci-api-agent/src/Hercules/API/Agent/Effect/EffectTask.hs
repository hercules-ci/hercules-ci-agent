{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Effect.EffectTask where

import Hercules.API.Prelude
import Hercules.API.Task (Task)

data EffectTask = EffectTask
  { id :: Id (Task EffectTask),
    derivationPath :: Text,
    logToken :: Text,
    inputDerivationOutputPaths :: [Text],
    token :: Text,
    projectId :: Id "project",
    projectPath :: Text,
    siteName :: Text,
    ownerName :: Text,
    repoName :: Text,
    ref :: Text,
    isDefaultBranch :: Bool
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)
