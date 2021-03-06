{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.TaskStatus where

import Hercules.API.Prelude

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
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)
