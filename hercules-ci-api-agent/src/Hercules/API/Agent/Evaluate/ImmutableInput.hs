{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.ImmutableInput where

import Hercules.API.Agent.Evaluate.ImmutableGitInput (ImmutableGitInput)
import Hercules.API.Prelude

-- | A location
data ImmutableInput
  = -- | A de facto immutable URL. Do not use with URLs locating mutable contents!
    --
    -- Translates to @builtins.fetchTarball@
    ArchiveUrl Text
  | -- | A git commit below a ref.
    --
    -- Translates to @builtins.fetchGit@
    Git ImmutableGitInput
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
