{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.SourceHostingSite.SourceHostingSite where

import Hercules.API.Prelude

-- | A source hosting site (example github for github.com) used for
-- source code, permissions, CI statuses, ...
data SourceHostingSite
  = SourceHostingSite
      { id :: Id SourceHostingSite,
        slug :: Name SourceHostingSite,
        displayName :: Text,
        newInstallationURL :: Maybe Text
      }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
