{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.EvaluateEvent.JobConfig where

import Hercules.API.Prelude

data JobConfig = JobConfig
  { sourceCaches :: Maybe [Text],
    binaryCaches :: Maybe [Text]
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)
