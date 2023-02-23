{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hercules.API.Attribute
  ( AttributeType (..),
    Attribute (..),
    AttributePath (..),
    attributePathFromString,
    attributePathToString,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Lens (at, (%~))
import qualified Data.Aeson as A
import Data.Aeson.Lens
import Data.Function ((&))
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

data Attribute a = Attribute
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
  parseUrlPiece = Right . AttributePath . attributePathFromString

  parseQueryParam = parseUrlPiece

instance ToParamSchema AttributePath where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Text)

instance ToHttpApiData AttributePath where
  toUrlPiece = toUrlPiece . attributePathToString . fromAttributePath

----------------------------------------

attributePathToString :: [Text] -> Text
attributePathToString l =
  l & map stringToNixIdentifier & T.intercalate "."

stringToNixIdentifier :: Text -> Text
stringToNixIdentifier s =
  if isNixSimpleId s
    then s
    else nixQuote s

isNixSimpleId :: Text -> Bool
isNixSimpleId t | T.null t = False
isNixSimpleId t
  | not
      ( let h = T.head t
         in (h >= 'a' && h <= 'z')
              || (h >= 'A' && h <= 'Z')
              || h == '_'
      ) =
      False
isNixSimpleId t =
  T.all
    ( \c ->
        (c >= 'a' && c <= 'z')
          || (c >= 'A' && c <= 'Z')
          || (c >= '0' && c <= '9')
          || c == '_'
          || c == '\''
          || c == '-'
    )
    t

nixQuote :: Text -> Text
nixQuote s =
  "\""
    <> T.replace "\"" "\\\"" (T.replace "\\" "\\\\" s)
    <> "\""

----------------------------------------

attributePathFromString :: Text -> [Text]
attributePathFromString "" = []
attributePathFromString t = t & T.unpack & attributePathFromStringImpl [] "" & reverse & fmap T.pack

attributePathFromStringImpl :: [String] -> String -> [Char] -> [String]
attributePathFromStringImpl = \accPath accAttr chars -> case chars of
  [] -> reverse accAttr : accPath
  ('.' : cs) -> attributePathFromStringImpl (reverse accAttr : accPath) "" cs
  ('\"' : cs) -> attributePathFromStringElement accPath "" cs
  (c : cs) -> attributePathFromStringImpl accPath (c : accAttr) cs

attributePathFromStringElement :: [String] -> String -> [Char] -> [String]
attributePathFromStringElement = \accPath accAttr chars -> case chars of
  ('"' : cs) -> attributePathFromStringImpl accPath accAttr cs
  ('\\' : c : cs) -> attributePathFromStringElement (accPath) (c : accAttr) cs
  [] -> reverse accAttr : accPath
  (c : cs) -> attributePathFromStringElement accPath (c : accAttr) cs
