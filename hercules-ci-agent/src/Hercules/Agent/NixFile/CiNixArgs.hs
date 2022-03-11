{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Hercules.Agent.NixFile.CiNixArgs where

import Data.Aeson (ToJSON)
import Hercules.Agent.NixFile.GitSource (GitSource)
import Hercules.CNix.Expr (ToRawValue, ViaJSON (ViaJSON))
import Protolude

data CiNixArgs = CiNixArgs
  { src :: GitSource
  }
  deriving (Generic, ToJSON)
  deriving (ToRawValue) via (ViaJSON CiNixArgs)
