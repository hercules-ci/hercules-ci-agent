{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Build.DerivationEvent.BuiltOutput where

import Data.OpenApi qualified as O3
import Hercules.API.Prelude

data BuiltOutput = BuiltOutput
  { outputName :: Text,
    outputPath :: Text,
    hash :: Text,
    size :: Int64
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
