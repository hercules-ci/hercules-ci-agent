{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.GitLab.InstallationBuilder where

import Data.OpenApi qualified as O3
import Hercules.API.Error (Error)
import Hercules.API.Forge.SimpleForge (SimpleForge)
import Hercules.API.Prelude

data InstallationBuilder = InstallationBuilder
  { id :: Id InstallationBuilder,
    gitlabURL :: Text,
    name :: Text,
    displayName :: Text,
    forge :: Maybe SimpleForge,
    errors :: [Error]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)

data InstallationBuilders = InstallationBuilders
  { items :: [InstallationBuilder]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
