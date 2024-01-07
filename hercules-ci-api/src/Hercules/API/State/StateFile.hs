{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.State.StateFile where

import Data.OpenApi qualified as O3
import Hercules.API.Prelude
import Hercules.API.Projects.Project (Project)
import Hercules.API.State.StateVersion (StateVersion)

data StateFile = StateFile
  { projectId :: Id Project,
    name :: Text,
    versions :: [StateVersion]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
