{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Build.DerivationInfo where

import Data.OpenApi qualified as O3
import Hercules.API.Build.DerivationEvent (DerivationEvent)
import Hercules.API.Build.DerivationInfo.DerivationInput (DerivationInput)
import Hercules.API.Build.DerivationInfo.DerivationOutput (DerivationOutput)
import Hercules.API.Derivation (DerivationStatus)
import Hercules.API.Prelude
import Hercules.API.Projects.Job (Job)
import Hercules.API.Projects.Project (Project)

data DerivationInfo = DerivationInfo
  { status :: DerivationStatus,
    viaJob :: Maybe Job,
    viaProject :: Maybe Project,
    platform :: Text,
    requiredSystemFeatures :: [Text],
    inputDerivations :: [DerivationInput],
    outputs :: [DerivationOutput],
    usedInProjects :: [Project],
    events :: [[DerivationEvent]],
    mayRestart :: Bool,
    mayCancel :: Bool,
    dummy :: Maybe DerivationEvent -- TODO: remove and update/fix codegen
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
