{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.GitLab.CreateInstallationBuilderRequest where

import Hercules.API.Prelude

data CreateInstallationBuilderRequest = CreateInstallationBuilderRequest
  { gitlabURL :: Text,
    gitlabAdminUsername :: Text,
    gitlabAdminPassword :: Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
