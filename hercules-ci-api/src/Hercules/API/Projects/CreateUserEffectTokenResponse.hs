{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hercules.API.Projects.CreateUserEffectTokenResponse where

import Hercules.API.Prelude

data CreateUserEffectTokenResponse = CreateUserEffectTokenResponse
  { token :: Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON, ToSchema)
