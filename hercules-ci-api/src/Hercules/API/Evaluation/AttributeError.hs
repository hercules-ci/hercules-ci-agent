{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Evaluation.AttributeError where

import Data.OpenApi qualified as O3
import Hercules.API.Prelude

data AttributeError = AttributeError
  { errorMessage :: Text,
    -- | Not intended to be displayed to the user. This is
    --   @Just "BuildException"@ for evaluation time build failures.
    errorType :: Maybe Text,
    errorDerivation :: Maybe Text,
    trace :: Maybe Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
