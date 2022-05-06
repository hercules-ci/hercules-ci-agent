{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeIFDEvent where

import Hercules.API.Prelude

data AttributeIFDEvent = AttributeIFDEvent
  { expressionPath :: [Text],
    derivationPath :: Text,
    derivationOutput :: Text,
    done :: Bool,
    index :: Int
  }
  deriving (Generic, Show, Eq, NFData, FromJSON, ToJSON)
