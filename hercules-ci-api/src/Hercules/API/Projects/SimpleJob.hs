{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Projects.SimpleJob where

import Hercules.API.Prelude
import Hercules.API.Projects.Job (Job)
import Hercules.API.Projects.Project (Project)

data SimpleJob
  = SimpleJob
      { id :: Id Job,
        project :: Project,
        index :: Int64
      }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
