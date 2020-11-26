module Hercules.Formats.NixCache where

import Data.Aeson
import Data.Text (Text)
import Hercules.Formats.Common
  ( noVersion,
    withKind,
    withVersions,
  )
import Prelude

-- | Credentials and keys for a cache.
data NixCache = NixCache
  { storeURI :: Text,
    signingKeys :: [Text],
    publicKeys :: [Text]
  }

instance ToJSON NixCache where
  toJSON a =
    object $
      [ "kind" .= String "NixCache",
        "storeURI" .= storeURI a,
        "signingKeys" .= signingKeys a,
        "publicKeys" .= publicKeys a
      ]

  toEncoding a =
    pairs
      ( "kind"
          .= String "NixCache"
          <> "storeURI"
          .= storeURI a
          <> "signingKeys"
          .= signingKeys a
          <> "publicKeys"
          .= publicKeys a
      )

instance FromJSON NixCache where
  parseJSON =
    withKind "NixCache" $
      withVersions
        [ noVersion $ \o ->
            NixCache
              <$> o .: "storeURI"
              <*> o .: "signingKeys"
              <*> o .: "publicKeys"
        ]
