{-# LANGUAGE PolyKinds #-}

module Hercules.API.Id
  ( Id (..),
    idText,
    uncheckedCast,
  )
where

import Control.Lens ((?~))
import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.Function ((&))
import Data.Hashable (Hashable (..))
import Data.Swagger
  ( NamedSchema (NamedSchema),
    ParamSchema,
    SwaggerType (SwaggerString),
    ToParamSchema (..),
    ToSchema (..),
    format,
    paramSchemaToSchema,
    type_,
  )
import Data.Text (Text)
import qualified Data.UUID as UUID
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Web.HttpApiData
import Prelude

newtype Id (a :: k) = Id {idUUID :: UUID}
  deriving (Generic, Eq, Ord)

instance Hashable (Id a) where
  hashWithSalt s (Id uuid) =
    let (a, b, c, d) = UUID.toWords uuid
     in s
          `hashWithSalt` a
          `hashWithSalt` b
          `hashWithSalt` c
          `hashWithSalt` d

idText :: Id a -> Text
idText = UUID.toText . idUUID

uncheckedCast :: Id a -> Id b
uncheckedCast (Id s) = Id s

instance Show (Id a) where
  showsPrec n = showsPrec n . idText

instance ToJSON (Id a) where

  toEncoding = toEncoding . idText

  toJSON = toJSON . idText

instance FromJSON (Id a) where
  parseJSON = fmap Id . parseJSON

instance ToJSONKey (Id a) where
  toJSONKey = toJSONKeyText idText

instance FromJSONKey (Id a) where
  fromJSONKey = FromJSONKeyTextParser $ \text ->
    case UUID.fromText text of
      Just x -> pure $ Id x
      Nothing -> fail "Expected UUID"

instance ToHttpApiData (Id a) where
  toUrlPiece = idText

instance FromHttpApiData (Id a) where
  parseUrlPiece = fmap Id . parseUrlPiece

instance ToSchema (Id a) where
  declareNamedSchema = pure . NamedSchema Nothing . paramSchemaToSchema

instance ToParamSchema (Id a) where
  toParamSchema _ =
    (mempty :: ParamSchema t)
      & type_ ?~ SwaggerString
      & format ?~ "uuid"
