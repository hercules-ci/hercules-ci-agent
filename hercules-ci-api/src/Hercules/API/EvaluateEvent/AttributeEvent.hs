{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ConstraintKinds #-}
module Hercules.API.EvaluateEvent.AttributeEvent where

import           Prelude
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           Data.Swagger                   ( ToSchema )

data AttributeEvent = AttributeEvent
  { expressionPath :: [Text]
  , derivationPath :: Text
  -- TODO: metadata
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
