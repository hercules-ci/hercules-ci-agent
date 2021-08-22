{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.SourceHostingSite.SimpleSite where

import Hercules.API.Prelude
import Hercules.API.SourceHostingSite.SourceHostingSite (SourceHostingSite)

data SimpleSite = SimpleSite
  { id :: Id SourceHostingSite,
    name :: Name SourceHostingSite,
    displayName :: Text
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
