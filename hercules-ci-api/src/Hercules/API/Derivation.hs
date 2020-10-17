{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Derivation where

import Hercules.API.Prelude hiding (either)

data DerivationPath
  = DerivationPath
      { drvPath :: Text
      }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)

data Derivation
  = Derivation
      { status :: DerivationStatus,
        derivationPath :: Text
      }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)

data DerivationStatus
  = Waiting
  | Building
  | BuildFailure
  | DependencyFailure
  | BuildSuccess
  | Cancelled
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
