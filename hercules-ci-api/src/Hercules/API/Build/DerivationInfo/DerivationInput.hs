{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Build.DerivationInfo.DerivationInput where

import Data.OpenApi qualified as O3
import Hercules.API.Derivation (DerivationStatus)
import Hercules.API.Prelude

data DerivationInput = DerivationInput
  { derivationStatus :: DerivationStatus,
    derivationPath :: Text,
    outputName :: Text,
    outputPath :: Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
