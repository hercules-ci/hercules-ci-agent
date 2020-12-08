{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Build.BuildTask where

import Hercules.API.Prelude
import Hercules.API.Task (Task)

data BuildTask = BuildTask
  { id :: Id (Task BuildTask),
    derivationPath :: Text,
    logToken :: Text,
    inputDerivationOutputPaths :: [Text]
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)
