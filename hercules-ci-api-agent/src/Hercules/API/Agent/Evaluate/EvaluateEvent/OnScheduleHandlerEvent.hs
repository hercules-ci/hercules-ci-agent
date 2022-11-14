{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.EvaluateEvent.OnScheduleHandlerEvent where

import Hercules.API.Agent.Evaluate.EvaluateEvent.InputDeclaration (InputDeclaration)
import Hercules.API.DayOfWeek (DayOfWeek)
import Hercules.API.Prelude

data OnScheduleHandlerEvent = OnScheduleHandlerEvent
  { handlerName :: Text,
    handlerExtraInputs :: Map Text InputDeclaration,
    isFlake :: Bool,
    when :: TimeConstraints
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)

data TimeConstraints = TimeConstraints
  { minute :: Maybe Int,
    hour :: Maybe [Int],
    dayOfWeek :: Maybe [DayOfWeek],
    dayOfMonth :: Maybe [Int]
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)
