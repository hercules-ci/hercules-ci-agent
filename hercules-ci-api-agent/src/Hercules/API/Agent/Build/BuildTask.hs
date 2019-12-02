{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Build.BuildTask where

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

data BuildTask
  = BuildTask
      { id :: Id (Task BuildTask),
        derivationPath :: Text,
        logToken :: Text
      }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
