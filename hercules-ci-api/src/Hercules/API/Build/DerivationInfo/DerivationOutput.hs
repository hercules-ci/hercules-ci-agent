{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Build.DerivationInfo.DerivationOutput where

import Hercules.API.Prelude

data DerivationOutput
  = DerivationOutput
      { outputName :: Text,
        outputPath :: Text
      }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
