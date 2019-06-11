module Hercules.Formats.CacheKeys where

import           Prelude
import           Data.Map                       ( Map )
import           Data.Text                      ( Text )
import           Hercules.Formats.CacheKeys.Keys
                                                ( Keys )
import           Hercules.Formats.Common
import           Data.Aeson

-- | File format that stores keys for multiple caches; both credentials and
-- public keys.
--
-- This data is sensitive with respect to both confidentiality and integrity.
data CacheKeys = CacheKeys
  { caches :: Map Text Keys
  }

instance ToJSON CacheKeys where
  toJSON a = object ["kind" .= String "CacheKeys", "caches" .= caches a]
  toEncoding a = pairs ("kind" .= String "CacheKeys" <> "caches" .= caches a)

instance FromJSON CacheKeys where
  parseJSON = withKind "CacheKeys"
    $ withVersions [noVersion $ \o -> CacheKeys <$> o .: "caches"]
