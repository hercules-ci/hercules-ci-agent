{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.EvaluateEvent.BuildRequired where

import Hercules.API.Prelude

data BuildRequired
  = BuildRequired
      { index :: Int,
        derivationPath :: Text,
        outputName :: Text
      }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
