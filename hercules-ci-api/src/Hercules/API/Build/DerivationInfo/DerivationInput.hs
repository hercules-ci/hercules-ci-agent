{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Build.DerivationInfo.DerivationInput where

import Hercules.API.Derivation (DerivationStatus)
import Hercules.API.Prelude

data DerivationInput
  = DerivationInput
      { derivationStatus :: DerivationStatus,
        derivationPath :: Text,
        outputName :: Text,
        outputPath :: Text
      }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
