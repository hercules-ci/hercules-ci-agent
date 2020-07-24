{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Effect.EffectTask where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hercules.API.Id
import Hercules.API.Task (Task)
import Prelude

data EffectTask
  = EffectTask
      { id :: Id (Task EffectTask),
        derivationPath :: Text,
        logToken :: Text,
        inputDerivationOutputPaths :: [Text]
      }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)