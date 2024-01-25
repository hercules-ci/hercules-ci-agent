{-# LANGUAGE ScopedTypeVariables #-}

-- | JSON codecs using the tomland method
module Hercules.Agent.Json where

import Control.Lens
  ( Prism',
    Traversal',
    at,
    re,
    (%~),
    (^.),
    (^?),
  )
import Control.Monad.Trans.Maybe
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens qualified as Aeson.Lens
import Data.Aeson.Text qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Protolude hiding (to)
import Toml hiding
  ( decode,
    decodeFile,
    match,
  )

-- TODO: upstream this

type JsonEnv = ExceptT Text (Reader Aeson.Value)

type JsonSt = MaybeT (State Aeson.Value)

type JsonCodec a = BiCodec JsonEnv JsonSt a

type JsonBiMap = BiMap Text

-- | Decode a value from a file. In case of parse errors, throws 'LoadTomlException'.
decodeFile :: (MonadIO m) => JsonCodec a -> FilePath -> m a
decodeFile codec filePath =
  liftIO $ (decode codec <$> BL.readFile filePath) >>= errorWhenLeft
  where
    errorWhenLeft :: Either Text a -> IO a
    errorWhenLeft (Left e) =
      throwIO $ FatalError $ "In JSON file " <> show filePath <> ": " <> e
    errorWhenLeft (Right pc) = pure pc

-- | Convert textual representation of JSON into user data type.
decode :: JsonCodec a -> BL.ByteString -> Either Text a
decode codec txt = do
  toml <- first toS $ Aeson.eitherDecode txt
  runCodec codec toml

-- | Convert JSON into user data type.
runCodec :: JsonCodec a -> Aeson.Value -> Either Text a
runCodec codec = runReader (runExceptT $ codecRead codec)

-- | Convert JSON to textual representation.
encode :: JsonCodec a -> a -> TL.Text
encode codec obj = Aeson.encodeToLazyText $ execCodec codec obj

-- | Runs 'codecWrite' of 'JsonCodec' and returns intermediate Aeson AST.
execCodec :: JsonCodec a -> a -> Aeson.Value
execCodec codec obj =
  execState (runMaybeT $ codecWrite codec obj) (Aeson.Object mempty)

at' :: Key -> Traversal' Aeson.Value Aeson.Value
at' (Key ks) = foldr (\(Piece p) r -> Aeson.Lens.key p . r) identity ks

insertKeyValue :: Key -> Aeson.Value -> Aeson.Value -> Aeson.Value
insertKeyValue (Key ks) v =
  foldr
    (\(Piece p) r -> Aeson.Lens._Object . at p %~ s r)
    (const v)
    ks
  where
    s :: (Aeson.Value -> Aeson.Value) -> Maybe Aeson.Value -> Maybe Aeson.Value
    s setSub (Just x) = Just (setSub x)
    s setSub Nothing = Just (setSub (Aeson.Object mempty))

match :: forall a. JsonBiMap a Aeson.Value -> Key -> JsonCodec a
match bm key = Codec input output
  where
    input :: JsonEnv a
    input = do
      mVal <- asks $ (^? at' key)
      case mVal of
        Nothing -> throwError $ "Not found: " <> showKey key
        Just val -> case backward bm val of
          Right v -> pure v
          Left err -> throwError $ "In " <> showKey key <> ": " <> err

    output :: a -> JsonSt a
    output a = do
      val <- MaybeT $ pure $ either (const Nothing) Just $ forward bm a
      a <$ modify (insertKeyValue key val)

showKey :: Key -> Text
showKey (Key ps) = T.intercalate "." (map (toS . unPiece) (Protolude.toList ps))

text :: Key -> JsonCodec Text
text = match $ prismWithError "String expected" Aeson.Lens._String

integer :: Key -> JsonCodec Integer
integer = match $ prismWithError "Integer expected" Aeson.Lens._Integer

prismWithError :: e -> Prism' a b -> BiMap e b a
prismWithError e p =
  BiMap
    { forward = \x -> Right (x ^. re p),
      backward = \y -> maybeToEither e $ y ^? p
    }
