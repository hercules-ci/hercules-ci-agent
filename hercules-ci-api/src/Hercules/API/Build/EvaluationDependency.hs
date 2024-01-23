{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Build.EvaluationDependency where

import Data.OpenApi qualified as O3
import Hercules.API.Derivation (Derivation)
import Hercules.API.Prelude

data EvaluationDependency = EvaluationDependency
  { index :: Int,
    derivation :: Derivation,
    outputName :: Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
