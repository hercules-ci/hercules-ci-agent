{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Error where

import Data.OpenApi qualified as O3
import Hercules.API.Prelude

-- | General error type used in (some) HTTP error response bodies and in some
-- resources.
data Error = Error
  { -- | Symbolic names of the error condition; identifiers that clients
    -- may use to identify specific errors or classes of errors.
    errorTags :: [Text],
    -- | Human-readable error message.
    message :: Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
