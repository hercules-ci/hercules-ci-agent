{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ConstraintKinds #-}
module Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeErrorEvent where

import           Prelude
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           Data.Swagger                   ( ToSchema )

data AttributeErrorEvent = AttributeErrorEvent
  { expressionPath :: [Text]
  , errorMessage :: Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
