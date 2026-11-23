module Hercules.Formats.AtticCache where

import Data.Aeson
import Data.Foldable
import Data.Text (Text)
import Hercules.Formats.Common
  ( noVersion,
    withKind,
    withVersions,
  )
import Prelude

data AtticCache = AtticCache
  { serverEndpoint :: Text,
    cacheName :: Text,
    token :: Text,
    publicKeys :: [Text]
  }

instance ToJSON AtticCache where
  toJSON a =
    object $
      [ "kind" .= String "AtticCache",
        "serverEndpoint" .= serverEndpoint a,
        "cacheName" .= cacheName a,
        "token" .= token a
      ]
        <> ["publicKeys" .= publicKeys a]

  toEncoding a =
    pairs
      ( "kind"
          .= String "AtticCache"
          <> "serverEndpoint"
            .= serverEndpoint a
          <> "cacheName"
            .= cacheName a
          <> "token"
            .= token a
          <> "publicKeys"
            .= publicKeys a
      )

instance FromJSON AtticCache where
  parseJSON =
    withKind "AtticCache" $
      withVersions
        [ noVersion $ \o ->
            AtticCache
              <$> o .: "serverEndpoint"
              <*> o .: "cacheName"
              <*> o .: "token"
              <*> (fold <$> o .:? "publicKeys")
        ]
