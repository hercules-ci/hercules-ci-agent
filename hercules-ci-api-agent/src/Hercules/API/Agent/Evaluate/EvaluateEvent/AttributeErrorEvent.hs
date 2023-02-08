{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeErrorEvent where

import Data.Aeson.Types
import Hercules.API.Prelude

data AttributeErrorEvent = AttributeErrorEvent
  { expressionPath :: [Text],
    errorMessage :: Text,
    errorDerivation :: Maybe Text,
    errorType :: Maybe Text,
    trace :: Maybe Text
  }
  deriving (Generic, Show, Eq, NFData)

-- Smoke test
instance ToJSON AttributeErrorEvent where
  toJSON = genericToJSON aesonOptions

  toEncoding = genericToEncoding aesonOptions

instance FromJSON AttributeErrorEvent where
  parseJSON = genericParseJSON aesonOptions

aesonOptions :: Options
aesonOptions =
  defaultOptions
    { omitNothingFields = True
    }
