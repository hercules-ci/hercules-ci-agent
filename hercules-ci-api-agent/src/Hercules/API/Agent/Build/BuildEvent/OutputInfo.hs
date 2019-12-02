{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Build.BuildEvent.OutputInfo where

import Hercules.API.Prelude

data OutputInfo
  = OutputInfo
      { -- | store path ending in .drv
        deriver :: Text,
        -- | e.g. out, dev
        name :: Text,
        -- | store path
        path :: Text,
        -- | typically sha256:...
        hash :: Text,
        -- | nar size in bytes
        size :: Integer
      }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
