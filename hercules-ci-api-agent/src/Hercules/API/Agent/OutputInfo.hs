{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.OutputInfo where

import Hercules.API.Prelude

-- | Produced by both build tasks and evaluation tasks
data OutputInfo = OutputInfo
  { -- | store path ending in .drv
    deriver :: Text,
    -- | e.g. out, dev
    name :: Text,
    -- | store path
    path :: Text,
    -- | typically sha256:...
    hash :: Text,
    -- | nar size in bytes
    size :: Integer,
    -- | output references in store path basename form
    --
    -- Maybe for backcompat; older agents have not reported the output references.
    references :: Maybe [Text]
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)
