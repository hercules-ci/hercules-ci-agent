{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.EvaluateEvent.InputDeclaration where

import Hercules.API.Prelude

data InputDeclaration
  = SiblingInput SiblingInput
  | -- | Only exists to ensure correct serialization format. Will be replaced by a real second constructor.
    BogusInput Text
  deriving (Generic, Show, Eq, NFData, FromJSON, ToJSON)

data SiblingInput = MkSiblingInput
  { project :: Text,
    ref :: Maybe Text
  }
  deriving (Generic, Show, Eq, NFData, FromJSON, ToJSON)
