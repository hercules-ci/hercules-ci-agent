{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Build.DerivationEvent.BuiltOutput where

import Hercules.API.Prelude

data BuiltOutput = BuiltOutput
  { outputName :: Text,
    outputPath :: Text,
    hash :: Text,
    size :: Int64
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
