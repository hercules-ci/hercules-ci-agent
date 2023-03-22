{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Build.DerivationInfo.DerivationInput where

import Hercules.API.Derivation (DerivationStatus)
import Hercules.API.Prelude

data DerivationInput = DerivationInput
  { derivationStatus :: DerivationStatus,
    derivationPath :: Text,
    outputName :: Text,
    outputPath :: Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
