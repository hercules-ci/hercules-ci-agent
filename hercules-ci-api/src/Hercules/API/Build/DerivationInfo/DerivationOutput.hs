{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Build.DerivationInfo.DerivationOutput where

import Data.OpenApi qualified as O3
import Hercules.API.Prelude

data DerivationOutput = DerivationOutput
  { outputName :: Text,
    outputPath :: Maybe Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
