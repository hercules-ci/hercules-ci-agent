{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Build.Log where

import Hercules.API.Build.LogLine
import Hercules.API.Prelude

data Log = Log
  { id :: Id "log",
    lines :: [LogLine],
    done :: Bool
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
