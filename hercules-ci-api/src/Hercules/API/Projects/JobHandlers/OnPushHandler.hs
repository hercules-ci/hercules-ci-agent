{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Projects.JobHandlers.OnPushHandler
  ( OnPushHandler (..),
  )
where

import Hercules.API.Prelude

data OnPushHandler = OnPushHandler
  { name :: Text,
    isFlake :: Bool
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
