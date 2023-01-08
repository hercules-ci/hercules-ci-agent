{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Effects.EffectInfo where

import Hercules.API.Build.DerivationInfo.DerivationInput (DerivationInput)
import Hercules.API.Effects.EffectEvent (EffectEvent)
import Hercules.API.Effects.EffectReference (EffectReference)
import Hercules.API.Prelude
import Hercules.API.Projects.Job (Job)
import Hercules.API.Projects.Project (Project)

data EffectStatus
  = Waiting
  | Running
  | Failed
  | DependencyFailed
  | Successful
  | Cancelled
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)

data EffectInfo = EffectInfo
  { status :: EffectStatus,
    jobId :: Id Job,
    projectId :: Id Project,
    platform :: Text,
    requiredSystemFeatures :: [Text],
    inputDerivations :: [DerivationInput],
    events :: [[EffectEvent]],
    waitingForEffects :: [EffectReference],
    mayCancel :: Bool,
    dummy :: Maybe EffectEvent -- TODO: remove and update/fix codegen
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
