{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.TaskStatus where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

-- | Similar to a process exit code.
--
-- User feedback in case of an error must be communicated out of band
data TaskStatus
  = -- | Everything was ok.
    Successful ()
  | -- | We did what we could but dependents can not continue.
    Terminated ()
  | -- | Some assumption in the software failed.
    Exceptional Text
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
