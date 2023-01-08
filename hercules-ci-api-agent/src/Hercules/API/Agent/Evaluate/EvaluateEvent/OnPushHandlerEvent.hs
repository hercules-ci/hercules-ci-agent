{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.EvaluateEvent.OnPushHandlerEvent where

import Control.Applicative ((<|>))
import Control.Lens (at, (%~))
import qualified Data.Aeson as A
import Data.Aeson.Lens (_Object)
import Hercules.API.Agent.Evaluate.EvaluateEvent.InputDeclaration (InputDeclaration)
import Hercules.API.Prelude

data OnPushHandlerEvent = OnPushHandlerEvent
  { handlerName :: Text,
    handlerExtraInputs :: Map Text InputDeclaration,
    isFlake :: Bool
  }
  deriving (Generic, Show, Eq, NFData, ToJSON)

instance FromJSON OnPushHandlerEvent where
  parseJSON = A.genericParseJSON A.defaultOptions . fixup
    where
      fixup :: A.Value -> A.Value
      fixup = _Object . at "isFlake" %~ (<|> Just (A.Bool False))
