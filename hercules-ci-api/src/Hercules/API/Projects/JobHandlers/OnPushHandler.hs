{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Projects.JobHandlers.OnPushHandler
  ( OnPushHandler (..),
  )
where

import Hercules.API.Prelude

data OnPushHandler = OnPushHandler
  { name :: Text,
    isFlake :: Bool
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
