{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.GitLab.CreateInstallationBuilderResponse where

import Hercules.API.GitLab.InstallationBuilder (InstallationBuilder)
import Hercules.API.Prelude

data CreateInstallationBuilderResponse = CreateInstallationBuilderResponse
  { ok :: Maybe InstallationBuilder,
    error :: Maybe Text
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
