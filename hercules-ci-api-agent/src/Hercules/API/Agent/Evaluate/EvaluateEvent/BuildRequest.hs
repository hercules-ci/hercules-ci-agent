{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.EvaluateEvent.BuildRequest where

import Hercules.API.Prelude

data BuildRequest
  = BuildRequest
      { derivationPath :: Text
      }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
