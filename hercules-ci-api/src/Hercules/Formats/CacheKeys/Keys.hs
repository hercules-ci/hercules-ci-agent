module Hercules.Formats.CacheKeys.Keys where

import           Prelude

import           Data.Aeson
import           Data.Foldable
import           Data.Text                      ( Text )

-- | Credentials and public keys for a cache.
data Keys = Keys
 { signingKeys :: [Text]
 , pullToken :: Maybe Text
 , publicKeys :: [Text]
 }

instance ToJSON Keys where
  toJSON a =
    object
      $ ["signingKeys" .= signingKeys a]
      <> foldMap (pure . ("pullToken" .=)) (toList $ pullToken a)
      <> ["publicKeys" .= publicKeys a]

  toEncoding a = pairs
    ("signingKeys"
    .= signingKeys a
    <> foldMap ("pullToken" .=) (pullToken a)
    <> "publicKeys"
    .= publicKeys a
    )

instance FromJSON Keys where
  parseJSON = withObject "Keys" $ \o ->
    Keys <$> o .: "signingKeys" <*> o .:? "pullToken" <*> o .: "publicKeys"
