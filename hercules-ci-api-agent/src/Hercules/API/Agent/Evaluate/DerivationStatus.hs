{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.DerivationStatus where

import Hercules.API.Prelude

-- TODO simplify
data DerivationStatus
  = Waiting
  | Building
  | BuildFailure
  | DependencyFailure
  | BuildSuccess
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
