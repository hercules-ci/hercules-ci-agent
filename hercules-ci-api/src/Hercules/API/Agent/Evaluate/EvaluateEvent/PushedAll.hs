{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Agent.Evaluate.EvaluateEvent.PushedAll where

import           Hercules.API.Prelude

-- | Indicates that all derivations have been pushed to a cache.
data PushedAll = PushedAll
  { cache :: Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
