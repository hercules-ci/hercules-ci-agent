{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Projects.CreateUserEffectTokenResponse where

import Data.OpenApi qualified as O3
import Hercules.API.Prelude

data CreateUserEffectTokenResponse = CreateUserEffectTokenResponse
  { token :: Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema, O3.ToSchema)
