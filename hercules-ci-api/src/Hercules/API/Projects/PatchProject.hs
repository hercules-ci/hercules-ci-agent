{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Projects.PatchProject where

import Hercules.API.Prelude

-- | Changes to a Project. 'Nothing' represents no change in a field.
data PatchProject
  = PatchProject
      { enabled :: Maybe Bool
        }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
