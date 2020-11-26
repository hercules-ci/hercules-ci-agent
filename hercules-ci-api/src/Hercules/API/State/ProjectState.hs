{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.State.ProjectState where

import Hercules.API.Prelude
import Hercules.API.Projects.Project (Project)
import Hercules.API.State.StateFile (StateFile)

data ProjectState = ProjectState
  { projectId :: Id Project,
    stateFiles :: Map Text StateFile
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
