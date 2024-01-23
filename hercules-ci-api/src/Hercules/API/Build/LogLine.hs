{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Build.LogLine where

import Data.OpenApi qualified as O3
import Data.Word
import Hercules.API.Prelude

data LogLine = LogLine {i :: !Word64, ms :: !Word64, t :: !Text}
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
