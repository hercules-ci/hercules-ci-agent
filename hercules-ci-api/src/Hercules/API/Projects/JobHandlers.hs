{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Projects.JobHandlers
  ( JobHandlers (..),
  )
where

import Hercules.API.Prelude
import Hercules.API.Projects.Job (Job)
import Hercules.API.Projects.JobHandlers.OnPushHandler (OnPushHandler)
import Hercules.API.Projects.JobHandlers.OnScheduleHandler (OnScheduleHandler)

data JobHandlers = JobHandlers
  { jobId :: Id Job,
    onPush :: Map Text OnPushHandler,
    onSchedule :: Map Text OnScheduleHandler
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
