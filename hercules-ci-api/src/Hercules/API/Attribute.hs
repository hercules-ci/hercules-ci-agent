{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Hercules.API.Attribute where

import Control.Applicative (Alternative ((<|>)))
import Control.Lens ((%~), at)
import qualified Data.Aeson as A
import Data.Aeson.Lens
import Data.Proxy (Proxy (Proxy))
import Data.Swagger (ToParamSchema (..))
import qualified Data.Text as T
import Hercules.API.Prelude
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import Prelude ()

data AttributeType
  = Regular
  | MustFail
  | MayFail
  | DependenciesOnly
  | Effect
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, ToSchema)

-- | An arbitrary ordering
deriving instance Ord AttributeType

data Attribute a
  = Attribute
      { path :: [Text],
        value :: a,
        typ :: AttributeType
      }
  deriving (Generic, Show, Eq, NFData, ToJSON)

instance FromJSON a => FromJSON (Attribute a) where
  parseJSON v = A.parseJSON (fixup v)
    where
      fixup :: A.Value -> A.Value
      fixup = _Object . at "typ" %~ (<|> Just (A.String "Regular"))

deriving instance ToSchema a => ToSchema (Attribute a)

deriving instance Functor Attribute

deriving instance Foldable Attribute

deriving instance Traversable Attribute

newtype AttributePath = AttributePath {fromAttributePath :: [Text]}
  deriving (Generic, Eq, NFData)

instance FromHttpApiData AttributePath where
  -- TODO parse properly
  parseUrlPiece = Right . AttributePath . T.split (== '.')

  parseQueryParam = parseUrlPiece

instance ToParamSchema AttributePath where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Text)

instance ToHttpApiData AttributePath where
  toUrlPiece = toUrlPiece . T.intercalate "." . fromAttributePath
