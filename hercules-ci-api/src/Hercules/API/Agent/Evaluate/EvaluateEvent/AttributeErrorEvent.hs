{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ConstraintKinds #-}
module Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeErrorEvent where

import           Hercules.API.Prelude

data AttributeErrorEvent = AttributeErrorEvent
  { expressionPath :: [Text]
  , errorMessage :: Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
