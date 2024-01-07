{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Projects.PatchProject where

import Data.OpenApi qualified as O3
import Hercules.API.Prelude

-- | Changes to a Project. 'Nothing' represents no change in a field.
data PatchProject = PatchProject
  { enabled :: Maybe Bool
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
