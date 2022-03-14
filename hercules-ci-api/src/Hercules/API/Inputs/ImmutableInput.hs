{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Inputs.ImmutableInput where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toEncoding, toJSON),
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
  )
import Hercules.API.Inputs.ImmutableGitInput
import Hercules.API.Prelude

data ImmutableInput
  = GitInput ImmutableGitInput
  | IgnoreMe ()
  deriving (Generic, Show, Eq, NFData, ToSchema)

instance FromJSON ImmutableInput where
  parseJSON = genericParseJSON schemaCompatibleOptions

instance ToJSON ImmutableInput where
  toJSON = genericToJSON schemaCompatibleOptions

  toEncoding = genericToEncoding schemaCompatibleOptions
