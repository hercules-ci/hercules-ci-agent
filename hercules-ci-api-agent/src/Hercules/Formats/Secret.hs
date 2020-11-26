module Hercules.Formats.Secret where

import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import Hercules.Formats.Common
  ( noVersion,
    withKind,
    withVersions,
  )
import Prelude

-- | Arbitrary secret like keys, tokens, passwords etc.
data Secret = Secret
  { data_ :: Map Text Value
  }

instance ToJSON Secret where
  toJSON a =
    object $
      ["kind" .= String "Secret", "data" .= data_ a]

  toEncoding a =
    pairs
      ("kind" .= String "Secret" <> "data" .= data_ a)

instance FromJSON Secret where
  parseJSON =
    withKind "Secret" $
      withVersions
        [ noVersion $ \o ->
            Secret
              <$> o .: "data"
        ]
