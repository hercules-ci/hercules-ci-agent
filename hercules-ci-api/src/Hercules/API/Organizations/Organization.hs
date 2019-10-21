{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Organizations.Organization where

import Hercules.API.Prelude

data Organization
  = Organization
      { id :: Id Organization,
        displayName :: Text,
        trialStartedOn :: Maybe UTCTime
        }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
