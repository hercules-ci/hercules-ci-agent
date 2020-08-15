{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hercules.API.Attribute where

import Control.Applicative (Alternative ((<|>)))
import Control.Lens ((%~), at)
import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import qualified Data.Aeson as A
import Data.Aeson.Lens
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

data AttributeType
  = Regular
  | MustFail
  | MayFail
  | DependenciesOnly
  | Effect
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data Attribute a
  = Attribute
      { path :: [Text],
        value :: a,
        typ :: AttributeType
      }
  deriving (Generic, Show, Eq, ToJSON)

instance FromJSON a => FromJSON (Attribute a) where
  parseJSON v = A.parseJSON (fixup v)
    where
      fixup :: A.Value -> A.Value
      fixup = _Object . at "typ" %~ (<|> Just (A.String "Regular"))

deriving instance ToSchema a => ToSchema (Attribute a)

deriving instance Functor Attribute

deriving instance Foldable Attribute

deriving instance Traversable Attribute
