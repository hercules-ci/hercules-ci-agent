{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Evaluation.Evaluation where

import Data.OpenApi qualified as O3
import Hercules.API.Prelude

data Evaluation = Evaluation
  { id :: Id Evaluation
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
