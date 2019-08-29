{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Derivation where

import Data.Aeson
  ( FromJSON,
    ToJSON
    )
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (either)

data DerivationPath
  = DerivationPath
      { drvPath :: Text
        }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data Derivation
  = Derivation
      { status :: DerivationStatus,
        derivationPath :: Text
        }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data DerivationStatus
  = Waiting
  | Building
  | BuildFailure
  | DependencyFailure
  | BuildSuccess
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
