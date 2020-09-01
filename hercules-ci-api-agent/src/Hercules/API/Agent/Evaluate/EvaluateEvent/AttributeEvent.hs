{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEvent where

import Control.Applicative (Alternative ((<|>)))
import Control.Lens ((%~))
import Control.Lens.At (At (at))
import qualified Data.Aeson as A
import Data.Aeson.Lens (_Object)
import Hercules.API.Prelude

data AttributeType
  = Regular
  | MustFail
  | MayFail
  | DependenciesOnly
  | Effect
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

data AttributeEvent
  = AttributeEvent
      { expressionPath :: [Text],
        derivationPath :: Text,
        typ :: AttributeType
        -- TODO: meta attributes
      }
  deriving (Generic, Show, Eq, ToJSON)

instance FromJSON AttributeEvent where
  parseJSON v = A.genericParseJSON A.defaultOptions (fixup v)
    where
      fixup :: A.Value -> A.Value
      fixup = _Object . at "typ" %~ (<|> Just (A.String "Regular"))
