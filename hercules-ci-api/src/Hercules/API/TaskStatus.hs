{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.TaskStatus where

import Data.Aeson
  ( FromJSON,
    ToJSON
    )
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

-- | Similar to a process exit code.
--
-- User feedback in case of an error must be communicated out of band
data TaskStatus
  = Successful () -- ^ Everything was ok.
  | Terminated () -- ^ We did what we could but dependents can not continue.
  | Exceptional Text -- ^ Some assumption in the software failed.
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
