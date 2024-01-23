{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.GitLab.CreateInstallationBuilderRequest where

import Data.OpenApi qualified as O3
import Hercules.API.Prelude

data CreateInstallationBuilderRequest = CreateInstallationBuilderRequest
  { gitlabURL :: Text,
    gitlabAdminUsername :: Text,
    gitlabAdminPassword :: Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
