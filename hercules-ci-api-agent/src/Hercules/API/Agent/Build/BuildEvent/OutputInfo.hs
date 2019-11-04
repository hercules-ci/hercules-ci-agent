{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Build.BuildEvent.OutputInfo where

import Hercules.API.Prelude

data OutputInfo
  = OutputInfo
      { deriver :: Text, -- ^ store path ending in .drv
        name :: Text, -- ^ e.g. out, dev
        path :: Text, -- ^ store path
        hash :: Text, -- ^ typically sha256:...
        size :: Integer -- ^ nar size in bytes
        }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
