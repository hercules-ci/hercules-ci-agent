{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Projects.JobHandlers.OnScheduleHandler
  ( OnScheduleHandler (..),
    TimeConstraints (..),
  )
where

import Hercules.API.DayOfWeek (DayOfWeek)
import Hercules.API.Prelude

data OnScheduleHandler = OnScheduleHandler
  { name :: Text,
    isFlake :: Bool,
    when :: TimeConstraints,
    mainExists :: Bool
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)

data TimeConstraints = TimeConstraints
  { minute :: Int,
    hour :: [Int],
    dayOfWeek :: Maybe [DayOfWeek],
    dayOfMonth :: Maybe [Int]
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
