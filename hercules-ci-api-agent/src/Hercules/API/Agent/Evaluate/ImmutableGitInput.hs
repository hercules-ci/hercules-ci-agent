{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.ImmutableGitInput where

import Hercules.API.Prelude

data ImmutableGitInput = ImmutableGitInput
  { rev :: Text,
    ref :: Text,
    sshURL :: Text,
    httpURL :: Text
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
