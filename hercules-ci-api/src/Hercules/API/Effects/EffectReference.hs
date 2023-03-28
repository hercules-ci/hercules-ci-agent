{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Effects.EffectReference where

import Hercules.API.Prelude
import Hercules.API.Projects.SimpleJob (SimpleJob)

data EffectReference = EffectReference
  { job :: SimpleJob,
    attributePath :: [Text]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
