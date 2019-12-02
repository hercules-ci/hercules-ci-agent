{-# LANGUAGE PolyKinds #-}

module Hercules.API.Name
  ( Name (..),
    uncheckedCast,
  )
where

import Data.Aeson
import Data.Hashable (Hashable (..))
import Data.Proxy
import Data.Swagger
  ( ToParamSchema (..),
    ToSchema (..),
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Web.HttpApiData
import Prelude

-- | A slug. Display names are simply 'Text'.
newtype Name (a :: k) = Name {nameText :: Text}
  deriving (Generic, Eq, Ord)

instance Hashable (Name a)

uncheckedCast :: Name a -> Name b
uncheckedCast (Name s) = Name s

instance Show (Name a) where
  showsPrec n = showsPrec n . nameText

instance ToJSON (Name a) where

  toEncoding = toEncoding . nameText

  toJSON = toJSON . nameText

instance FromJSON (Name a) where
  parseJSON = fmap Name . parseJSON

instance ToHttpApiData (Name a) where
  toUrlPiece = nameText

instance FromHttpApiData (Name a) where
  parseUrlPiece = fmap Name . parseUrlPiece

instance ToSchema (Name a) where
  declareNamedSchema = declareNamedSchema . invmap nameText

instance ToParamSchema (Name a) where
  toParamSchema = toParamSchema . invmap nameText

invmap :: (a -> b) -> proxy a -> Proxy b
invmap _ _ = Proxy
