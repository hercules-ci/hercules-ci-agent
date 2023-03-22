{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Inputs.ImmutableGitInput where

import Hercules.API.Prelude
import Hercules.API.Projects.SimpleJob (SimpleJob)
import Hercules.API.Repos.SimpleRepo (SimpleRepo)

data ImmutableGitInput = ImmutableGitInput
  { rev :: Text,
    ref :: Text,
    sshURL :: Text,
    httpURL :: Text,
    repo :: Maybe SimpleRepo,
    jobs :: Maybe [SimpleJob],
    historyURL :: Maybe Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
