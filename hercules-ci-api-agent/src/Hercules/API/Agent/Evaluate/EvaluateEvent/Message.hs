{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.EvaluateEvent.Message where

import Hercules.API.Prelude

data Message = Message
  { index :: Int,
    typ :: Type,
    message :: Text
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)

data Type
  = -- | Something went wrong, inform user about possible
    -- cause. Examples: source could not be fetched, could not
    -- find a nix expression file to call.
    Error
  | -- | The nix expression contained a @builtins.trace@
    -- call. Ideally we should keep track of during which
    -- attribute it was encountered. It is not an attribute
    -- property because we can not reasonably know which
    -- attributes (plural) trigger the evaluation of
    -- @trace@. Indeed side effecting evaluation breaks the
    -- abstraction.
    Trace
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)
