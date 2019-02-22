{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Evaluation.Evaluation where

import           Hercules.API.Prelude

data Evaluation = Evaluation
  { id :: Id Evaluation
  -- TODO: attributes such as
  -- , isInProgress :: Bool
  -- , numberOfAttributes :: Int
  -- , numberOfEvaluationFailures :: Int
  -- , numberOfBuildFailures :: Int
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
