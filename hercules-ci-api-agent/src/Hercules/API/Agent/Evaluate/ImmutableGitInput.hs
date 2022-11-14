{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.ImmutableGitInput where

import Hercules.API.Prelude

data ImmutableGitInput = ImmutableGitInput
  { rev :: Text,
    ref :: Text,
    sshURL :: Text,
    httpURL :: Text,
    webURL :: Maybe Text,
    forgeType :: Maybe Text,
    owner :: Maybe Text,
    name :: Maybe Text
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
