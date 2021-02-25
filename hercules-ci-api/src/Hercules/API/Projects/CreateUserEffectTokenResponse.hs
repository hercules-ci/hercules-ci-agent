{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Projects.CreateUserEffectTokenResponse where

import Hercules.API.Prelude

data CreateUserEffectTokenResponse = CreateUserEffectTokenResponse
  { token :: Text
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)
