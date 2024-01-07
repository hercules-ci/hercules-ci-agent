{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Projects.JobHandlers.OnScheduleHandler
  ( OnScheduleHandler (..),
    TimeConstraints (..),
  )
where

import Data.OpenApi qualified as O3
import Hercules.API.DayOfWeek (DayOfWeek)
import Hercules.API.Prelude

data OnScheduleHandler = OnScheduleHandler
  { name :: Text,
    isFlake :: Bool,
    when :: TimeConstraints,
    mainExists :: Bool
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)

data TimeConstraints = TimeConstraints
  { minute :: Int,
    hour :: [Int],
    dayOfWeek :: Maybe [DayOfWeek],
    dayOfMonth :: Maybe [Int]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
