{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Evaluation.Evaluation where

import Hercules.API.Prelude

data Evaluation = Evaluation
  { id :: Id Evaluation
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
