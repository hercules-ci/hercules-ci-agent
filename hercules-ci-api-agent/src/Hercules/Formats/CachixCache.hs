module Hercules.Formats.CachixCache where

import Data.Aeson
import Data.Foldable
import Data.Text (Text)
import Hercules.Formats.Common
  ( noVersion,
    withKind,
    withVersions,
  )
import Prelude

-- | Credentials and keys for a cache.
data CachixCache = CachixCache
  { signingKeys :: [Text],
    authToken :: Maybe Text,
    publicKeys :: [Text]
  }

instance ToJSON CachixCache where
  toJSON a =
    object $
      ["kind" .= String "CachixCache", "signingKeys" .= signingKeys a]
        <> foldMap (pure . ("authToken" .=)) (toList $ authToken a)
        <> ["publicKeys" .= publicKeys a]

  toEncoding a =
    pairs
      ( "kind"
          .= String "CachixCache"
          <> "signingKeys"
            .= signingKeys a
          <> foldMap ("authToken" .=) (authToken a)
          <> "publicKeys"
            .= publicKeys a
      )

instance FromJSON CachixCache where
  parseJSON =
    withKind "CachixCache" $
      withVersions
        [ noVersion $ \o ->
            CachixCache
              <$> (fold <$> o .:? "signingKeys")
              <*> o
                .:? "authToken"
              <*> o
                .: "publicKeys"
        ]
