{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ConstraintKinds #-}
module Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEvent where

import           Hercules.API.Prelude

data AttributeEvent = AttributeEvent
  { expressionPath :: [Text]
  , derivationPath :: Text
  -- TODO: meta attributes
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
