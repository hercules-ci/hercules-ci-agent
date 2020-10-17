{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Build.LogLine where

import Data.Word
import Hercules.API.Prelude

data LogLine = LogLine {i :: !Word64, ms :: !Word64, t :: !Text}
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
