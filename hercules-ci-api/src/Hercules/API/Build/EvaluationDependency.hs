{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Build.EvaluationDependency where

import Hercules.API.Derivation (Derivation)
import Hercules.API.Prelude

data EvaluationDependency
  = EvaluationDependency
      { index :: Int,
        derivation :: Derivation,
        outputName :: Text
        }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
