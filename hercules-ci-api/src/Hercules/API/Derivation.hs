{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Derivation where

import Hercules.API.Prelude hiding (either)

data DerivationPath = DerivationPath
  { drvPath :: Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)

data Derivation = Derivation
  { status :: DerivationStatus,
    derivationPath :: Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)

data DerivationStatus
  = Waiting
  | Building
  | BuildFailure
  | DependencyFailure
  | BuildSuccess
  | Cancelled
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
