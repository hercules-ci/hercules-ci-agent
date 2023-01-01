{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Forge.Forge where

import Hercules.API.Prelude

-- | A source hosting site (example github for github.com) used for
-- source code, permissions, CI statuses, ...
data Forge = Forge
  { id :: Id Forge,
    slug :: Name Forge,
    displayName :: Text,
    typ :: Text,
    newInstallationURL :: Maybe Text,
    -- | Does the requesting user have admin permissions?
    adminPermission :: Maybe Bool
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
