{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Effects.EffectReference where

import Hercules.API.Prelude
import Hercules.API.Projects.SimpleJob (SimpleJob)

data EffectReference = EffectReference
  { job :: SimpleJob,
    attributePath :: [Text]
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
