{-# LANGUAGE ScopedTypeVariables #-}

-- | JSON codecs using the tomland method
--
-- NOTE: The write part of the codecs is largely untested as of yet, as it is
-- not used. It exists to match the tomland interface.
module Hercules.Agent.Config.Json where

import Control.Arrow (left)
import Control.Category ((>>>))
import Control.Lens
  ( Iso',
    Prism',
    Traversal',
    at,
    iso,
    re,
    (%~),
    (^.),
    (^?),
  )
import Control.Monad.Trans.Maybe
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as AM
import Data.Aeson.Lens qualified as Aeson.Lens
import Data.Aeson.Text qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as M
import Data.Profunctor (Profunctor (..), Star (..))
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V
import Numeric.Lens (integral)
import Protolude hiding (to)
import Toml hiding
  ( decode,
    decodeFile,
    dimap,
    first,
    match,
    tableMap,
    _EnumBounded,
    _Text,
    _TextBy,
  )

-- start codec

-- | Generic codec type.
data GCodec r w c a = GCodec
  { gRead :: r a,
    -- | `c -> w a`
    gWrite :: Star w c a
  }
  deriving (Functor)

instance (Applicative r, Applicative w) => Applicative (GCodec r w c) where
  pure x = GCodec (pure x) (pure x)
  GCodec r1 w1 <*> GCodec r2 w2 = GCodec (r1 <*> r2) (w1 <*> w2)

instance (Monad r, Monad w) => Monad (GCodec r w c) where
  (GCodec r w) >>= f =
    GCodec
      (r >>= gRead . f)
      (w >>= gWrite . f)

instance (Functor r, Functor w) => Profunctor (GCodec r w) where
  dimap f g (GCodec r w) = GCodec (fmap g r) (Data.Profunctor.dimap f g w)

instance (Alternative r, Alternative w) => Alternative (GCodec r w c) where
  empty = GCodec empty empty
  GCodec r1 w1 <|> GCodec r2 w2 = GCodec (r1 <|> r2) (w1 <|> w2)

-- end codec

type JsonEnv = ExceptT Text (Reader Aeson.Value)

type JsonSt = MaybeT (State Aeson.Value)

type JsonCodec = GCodec JsonEnv JsonSt

type JsonCodec' a = GCodec JsonEnv JsonSt a a

type JsonBiMap = BiMap Text

(.=.) :: (Profunctor p) => p field a -> (object -> field) -> p object a
a .=. f = dimap f identity a

proOptional :: (Alternative r, Applicative w) => GCodec r w c a -> GCodec r w (Maybe c) (Maybe a)
proOptional (GCodec rd wr) = GCodec (optional rd) (Star $ traverse (runStar wr))

-- | Decode a value from a file. In case of parse errors, throws 'LoadTomlException'.
decodeFile :: (MonadIO m) => JsonCodec' a -> FilePath -> m a
decodeFile codec filePath =
  liftIO $ (decode codec <$> BL.readFile filePath) >>= errorWhenLeft
  where
    errorWhenLeft :: Either Text a -> IO a
    errorWhenLeft (Left e) =
      throwIO $ FatalError $ "In JSON file " <> show filePath <> ": " <> e
    errorWhenLeft (Right pc) = pure pc

-- | Convert textual representation of JSON into user data type.
decode :: JsonCodec' a -> BL.ByteString -> Either Text a
decode codec txt = do
  toml <- first toS $ Aeson.eitherDecode txt
  runCodec codec toml

-- | Convert JSON into user data type.
runCodec :: JsonCodec' a -> Aeson.Value -> Either Text a
runCodec codec = runReader (runExceptT $ gRead codec)

-- | Convert JSON to textual representation.
encode :: JsonCodec' a -> a -> TL.Text
encode codec obj = Aeson.encodeToLazyText $ execCodec codec obj

-- | Runs 'codecWrite' of 'JsonCodec' and returns intermediate Aeson AST.
execCodec :: JsonCodec' a -> a -> Aeson.Value
execCodec codec obj =
  execState (runMaybeT $ runStar (gWrite codec) obj) (Aeson.Object mempty)

at' :: Key -> Traversal' Aeson.Value Aeson.Value
at' (Key ks) = foldr (\(Piece p) r -> Aeson.Lens.key (AK.fromText p) . r) identity ks

insertKeyValue :: Key -> Aeson.Value -> Aeson.Value -> Aeson.Value
insertKeyValue (Key ks) v =
  foldr
    (\(Piece p) r -> Aeson.Lens._Object . at (AK.fromText p) %~ s r)
    (const v)
    ks
  where
    s :: (Aeson.Value -> Aeson.Value) -> Maybe Aeson.Value -> Maybe Aeson.Value
    s setSub (Just x) = Just (setSub x)
    s setSub Nothing = Just (setSub (Aeson.Object mempty))

match :: forall a. JsonBiMap a Aeson.Value -> Key -> JsonCodec' a
match bm key = GCodec input (Star output)
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
showKey (Key ps) = T.intercalate "." (Protolude.map (toS . unPiece) (Protolude.toList ps))

text :: Key -> JsonCodec' Text
text = match $ prismWithError "String expected" Aeson.Lens._String

string :: Key -> JsonCodec' [Char]
string = match $ prismWithError "String expected" (Aeson.Lens._String . toS')

toS' :: Control.Lens.Iso' Text [Char]
toS' = Control.Lens.iso toS toS

integer :: Key -> JsonCodec' Integer
integer = match $ prismWithError "Integer expected" Aeson.Lens._Integer

int :: Key -> JsonCodec' Int
int = match $ prismWithError "Int expected" $ Aeson.Lens._Integer . integral

bool :: Key -> JsonCodec' Bool
bool = match $ prismWithError "Boolean expected" Aeson.Lens._Bool

value :: Key -> JsonCodec' Aeson.Value
value = match $ prismWithError "Value expected" identity

value' :: JsonCodec' Aeson.Value
value' = GCodec ask (Star (\x -> put x >> pure x))

dimatch ::
  (b -> Maybe a) ->
  (a -> b) ->
  JsonCodec' a ->
  JsonCodec' b
dimatch match_ make inner =
  GCodec
    { gRead = fmap make (gRead inner),
      gWrite = Star $ \b -> do
        case match_ b of
          Just a -> make <$> (runStar (gWrite inner) a)
          Nothing -> empty
    }

emap :: (e -> e2) -> BiMap e a b -> BiMap e2 a b
emap f (BiMap forward_ backward_) = BiMap (left f . forward_) (left f . backward_)

_EnumBounded :: (Show a, Enum a, Bounded a) => JsonBiMap a Aeson.Value
_EnumBounded = emap Toml.prettyBiMapError _EnumBoundedText >>> prismWithError "String expected" Aeson.Lens._String

_KeyText :: BiMap Text Key Text
_KeyText = emap Toml.prettyBiMapError Toml._KeyText

_Text :: BiMap Text Text Aeson.Value
_Text = prismWithError "String expected" Aeson.Lens._String

textBy :: (a -> Text) -> (Text -> Either Text a) -> Key -> JsonCodec' a
textBy to from_ = match (_TextBy to from_)

_TextBy ::
  forall a.
  (a -> Text) ->
  (Text -> Either Text a) ->
  BiMap Text a Aeson.Value
_TextBy toText parseText = BiMap toAnyValue fromAnyValue
  where
    toAnyValue :: a -> Either Text Aeson.Value
    toAnyValue = Right . Aeson.String . toText

    fromAnyValue :: Aeson.Value -> Either Text a
    fromAnyValue (Aeson.String v) = parseText v
    fromAnyValue _ = Left "String expected"

enumBounded :: (Bounded a, Enum a, Show a) => Key -> JsonCodec' a
enumBounded key = match (_EnumBounded) key

arrayOf :: forall a. BiMap Text a Aeson.Value -> Key -> JsonCodec' [a]
arrayOf m k = GCodec input (Star output)
  where
    input :: JsonEnv [a]
    input = do
      mVal <- asks $ (^? at' k)
      case mVal of
        Nothing -> throwError $ "Not found: " <> showKey k
        Just (Aeson.Array arr) -> do
          let go = traverse (backward m)
          case go (Protolude.toList arr) of
            Right v -> pure v
            Left err -> throwError $ "In " <> showKey k <> ": " <> err
        Just _ -> throwError $ "Array expected in " <> showKey k

    output :: [a] -> JsonSt [a]
    output as = do
      as' <- for as $ \a -> do
        MaybeT $ pure $ either (const Nothing) Just $ forward m a
      let arr = Aeson.Array . V.fromList $ as'
      modify (insertKeyValue k arr)
      pure as

tableMap' ::
  JsonCodec' v ->
  Key ->
  JsonCodec' (Map Text v)
tableMap' valCodec key =
  tableMap
    ( \k ->
        GCodec
          { gRead =
              local
                (\v -> fromMaybe (panic "tableMap': key disappeared") (v ^? at' k))
                (gRead valCodec),
            gWrite = do
              panic "tableMap': write not implemented"
          }
    )
    key

tableMap ::
  (Key -> JsonCodec' v) ->
  Key ->
  JsonCodec' (Map Text v)
tableMap valCodec key =
  let c = match (prismWithError "JSON Object expected" (Aeson.Lens._Object . aesonMap)) key
   in GCodec
        { gRead = do
            x <- gRead c
            fmap M.fromList $ for (M.toList x) $ \(k, _v) -> do
              v' <- gRead (valCodec (key <> (Key $ Piece k :| [])))
              pure (k, v'),
          gWrite = do
            panic "tableMap': write not implemented"
            -- for_ (M.toList m) $ \(k, v) -> do
            --   restore <- get
            --   put $ Aeson.object []
            --   _v <- runStar (gWrite valCodec) v
            --   v' <- get
            --   put restore
            --   modify (insertKeyValue k' v')
            -- pure _
        }

aesonMap :: Control.Lens.Iso' (AM.KeyMap a) (Map Text a)
aesonMap = Control.Lens.iso AM.toMapText AM.fromMapText

aesonMap' :: Control.Lens.Iso' (Map Text a) (AM.KeyMap a)
aesonMap' = Control.Lens.iso AM.fromMapText AM.toMapText

prismWithError :: e -> Prism' a b -> BiMap e b a
prismWithError e p =
  BiMap
    { forward = \x -> Right (x ^. re p),
      backward = \y -> maybeToEither e $ y ^? p
    }
