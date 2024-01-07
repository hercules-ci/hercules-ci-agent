{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Forge.Forge where

import Data.OpenApi qualified as O3
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
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
