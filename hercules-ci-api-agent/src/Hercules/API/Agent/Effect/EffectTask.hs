{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Effect.EffectTask where

import Hercules.API.Prelude
import Hercules.API.Task (Task)

data EffectTask
  = EffectTask
      { id :: Id (Task EffectTask),
        derivationPath :: Text,
        logToken :: Text,
        inputDerivationOutputPaths :: [Text],
        token :: Text
      }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
