{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.EvaluateEvent.BuildRequest where

import Control.Applicative
import Control.Lens ((%~), at)
import qualified Data.Aeson as A
import Data.Aeson.Lens (_Object)
import Hercules.API.Prelude

data BuildRequest
  = BuildRequest
      { derivationPath :: Text,
        forceRebuild :: Bool -- FIXME: API compatibility
      }
  deriving (Generic, Show, Eq, ToJSON, ToSchema)

instance FromJSON BuildRequest where
  parseJSON = A.genericParseJSON A.defaultOptions . fixup
    where
      fixup :: A.Value -> A.Value
      fixup = _Object . at "forceRebuild" %~ (<|> Just (A.Bool False))
