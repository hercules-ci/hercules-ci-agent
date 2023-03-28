{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Build.Log where

import Hercules.API.Build.LogLine
import Hercules.API.Prelude

data Log = Log
  { id :: Id "log",
    lines :: [LogLine],
    done :: Bool
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
