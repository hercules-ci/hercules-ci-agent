{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ConstraintKinds #-}
module Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeErrorEvent where

import           Hercules.API.Prelude
import           Data.Aeson.Types

data AttributeErrorEvent = AttributeErrorEvent
  { expressionPath :: [Text]
  , errorMessage :: Text
  , errorDerivation :: Maybe Text
  , errorType :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToSchema)
instance ToJSON AttributeErrorEvent where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions
instance FromJSON AttributeErrorEvent where
  parseJSON = genericParseJSON aesonOptions

aesonOptions :: Options
aesonOptions = defaultOptions
  { omitNothingFields = True
  }
