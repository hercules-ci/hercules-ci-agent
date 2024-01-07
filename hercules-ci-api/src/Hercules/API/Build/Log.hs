{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Build.Log where

import Data.OpenApi qualified as O3
import Hercules.API.Build.LogLine
import Hercules.API.Prelude

data Log = Log
  { id :: Id "log",
    lines :: [LogLine],
    done :: Bool
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
