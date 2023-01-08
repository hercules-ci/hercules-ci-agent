{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Forge.SimpleForge where

import Hercules.API.Forge.Forge (Forge)
import Hercules.API.Prelude

data SimpleForge = SimpleForge
  { id :: Id Forge,
    name :: Name Forge,
    displayName :: Text
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
