{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.EvaluateEvent.OnPushHandlerEvent where

import Hercules.API.Agent.Evaluate.EvaluateEvent.InputDeclaration (InputDeclaration)
import Hercules.API.Prelude

data OnPushHandlerEvent = OnPushHandlerEvent
  { handlerName :: Text,
    handlerExtraInputs :: Map Text InputDeclaration
  }
  deriving (Generic, Show, Eq, NFData, FromJSON, ToJSON)
