{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.GitLab.CreateInstallationBuilderRequest where

import Hercules.API.Prelude

data CreateInstallationBuilderRequest = CreateInstallationBuilderRequest
  { gitlabURL :: Text,
    gitlabAdminUsername :: Text,
    gitlabAdminPassword :: Text
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
