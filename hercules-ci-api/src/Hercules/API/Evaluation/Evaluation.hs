{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Evaluation.Evaluation where

import Hercules.API.Prelude

data Evaluation = Evaluation
  { id :: Id Evaluation
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
