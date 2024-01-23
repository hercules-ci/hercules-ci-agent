{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Projects.LegacySimpleJob where

import Data.OpenApi qualified as O3
import Hercules.API.Prelude
import Hercules.API.Projects.Project (Project)
import Hercules.API.Projects.SimpleJob (JobPhase, JobStatus)

-- | Legacy data type for hci < 0.3
data LegacySimpleJob = LegacySimpleJob
  { id :: Id "Job",
    project :: Project,
    index :: Int64,
    status :: JobStatus,
    phase :: JobPhase
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
