{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types and functions to represent interfaces between Nix code and Haskell
--     code.
module Hercules.CNix.Expr.Schema
  ( -- * Core
    PSObject (..),
    MonadEval,

    -- * Error handling
    Provenance (..),
    NixException (..),
    appendProvenance,

    -- * Alternatives

    --
    -- Runtime type matching. Use of @|@ comes from the implicit sum types that
    -- constitute Nix values.
    type (|.),
    (|!),

    -- * Functions
    type (->.),
    (.$),
    (>>$.),
    type (->?),
    ($?),
    (>>$?),

    -- * Simple types
    type StringWithoutContext,

    -- * Attribute sets
    basicAttrsWithProvenance,
    --
    -- Common type that can represent both simultaneously.
    type Attrs',

    -- * Attribute sets as records
    type Attrs,
    type (::.),
    (#.),
    (>>.),
    type (::?),
    (#?),
    (>>?),
    (#?!),

    -- * Attribute sets as used as dictionaries
    type Dictionary,
    dictionaryToMap,
    lookupDict,
    lookupDictBS,
    requireDict,
    requireDictBS,

    -- * Serialization
    toPSObject,
    FromPSObject (..),
    check,
    getText_,
    getByteString_,

    -- * Parsing Nix
    exprWithBasePath,
    exprWithBasePathBS,

    -- * Utilities
    uncheckedCast,
    englishOr,
  )
where

import Data.Coerce (coerce)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified GHC.TypeLits as TL
import Hercules.CNix.Expr (CheckType, EvalState, HasRawValueType, NixAttrs, NixFunction, NixPath, NixString, RawValue, Value (rtValue), apply, checkType, getAttr, getRawValueType, getStringIgnoreContext, hasContext, rawValueType, toRawValue, valueFromExpressionString)
import qualified Hercules.CNix.Expr as Expr
import Hercules.CNix.Expr.Raw (RawValueType, canonicalRawType)
import Protolude hiding (TypeError, check, evalState)

-- TODO add Pos fields
data Provenance
  = File FilePath
  | Other Text
  | Data
  | Attribute Provenance Text
  | Application Provenance Provenance
  deriving (Show, Eq, Ord)

data NixException
  = MissingAttribute Provenance Text
  | TypeError
      Provenance
      RawValueType
      -- ^ actual
      [RawValueType]
      -- ^ expected
  | InvalidText Provenance UnicodeException
  | StringContextNotAllowed Provenance
  deriving (Show, Eq)

instance Exception NixException where
  displayException (MissingAttribute p name) = "Missing attribute " <> show name <> appendProvenance p
  displayException (TypeError p actual expected) = "Expecting a value of type " <> toS (englishOr (map show expected)) <> ", but got type " <> show actual <> "." <> appendProvenance p
  displayException (InvalidText p ue) = displayException ue <> appendProvenance p
  displayException (StringContextNotAllowed p) = "This string must not have a context. It must be usable without building store paths." <> appendProvenance p

appendProvenance :: Provenance -> [Char]
appendProvenance (Attribute p name) = "\n  in attribute " <> show name <> appendProvenance p
appendProvenance (Other x) = "\n  in " <> toS x
appendProvenance Data = ""
appendProvenance (Application p _p) = "\n  in function result" <> appendProvenance p
appendProvenance (File f) = "\n  in file " <> show f

-- | Alternative schema. The value only needs to satisfy one subschema.
data a |. b

-- | Function schema.
data a ->. b

infixr 1 ->.

-- | Optional function. If the value is not a function, use it as the result.
type a ->? b = (a ->. b) |. b

infixr 1 ->?

-- | Attribute set schema with known attributes and wildcard type for remaining attributes.
data Attrs' (as :: [Attr]) w

-- | Attribute set schema with known attributes only
type Attrs as = Attrs' as Void

-- | Attribute set functioning as a "dictionary" from string keys to a certain type.
type Dictionary = Attrs' '[]

-- | A kind for attribute declarations.
data Attr
  = -- | Required attribute. Use '::.'.
    Symbol :. Type
  | -- | Optional attribute. Use ':?.'.
    Symbol :? Type

data StringWithoutContext

infix 0 :.

infix 0 :?

infix 0 ::.

infix 0 ::?

-- | Optional (@_?@) attribute name and type (@::_@)
--
-- This indicates that the attribute may be omitted in its entirety, which is
-- distinct from an attribute that may be @null@.
type a ::? b = a ':? b

-- | Required (@_.@) attribute name and type (@::_@)
--
-- Note that the type may still be nullable, but the attribute is expected to exist.
type a ::. b = a ':. b

-- | An object (thunk or value) with its 'Provenance' and an expected schema type attached as a
-- phantom type.
--
-- The phantom specifies the expactation, not a checked type.
data PSObject (a :: Type) = PSObject
  { -- | Tracks the origin of the object, which is useful informaton for error messages.
    provenance :: Provenance,
    -- | The Nix object, which may be a thunk (producing errors, non-termination, etc) or a 'Value' of any type.
    --
    -- Use 'check' and/or '|.' to evaluate it (whnf) and narrow down its runtime type to a specific 'Value'.
    value :: RawValue
  }

(.$) :: (MonadIO m) => PSObject (a ->. b) -> PSObject a -> m (PSObject b)
f .$ a = do
  v <- liftIO (value f `apply` value a)
  pure
    PSObject
      { provenance = Application (provenance f) (provenance a),
        value = v
      }

type AttrType as s = AttrType' as as s

type family AttrType' all as s where
  AttrType' all ((s ':. t) ': as) s = t
  AttrType' all ((s ':? t) ': as) s =
    TL.TypeError
      ( 'TL.Text "The attribute set field named " 'TL.:<>: 'TL.ShowType s 'TL.:<>: 'TL.Text " is optional."
          'TL.:$$: 'TL.Text "  Try the optional variation, e.g. (#?) instead of (#.)"
      )
  AttrType' all (_ ': as) s = AttrType' all as s
  AttrType' all '[] s =
    TL.TypeError
      ( 'TL.Text "Schema for attribute set does not declare a field named " 'TL.:<>: 'TL.ShowType s 'TL.:<>: 'TL.Text "."
          'TL.:$$: 'TL.Text "  Known attributes are " 'TL.:<>: 'TL.ShowType all
      )

type OptionalAttrType as s = OptionalAttrType' as as s

type family OptionalAttrType' all as s where
  OptionalAttrType' all ((s ':? t) ': as) s = t
  OptionalAttrType' all ((s ':. t) ': as) s =
    TL.TypeError
      ( 'TL.Text "The attribute set field named " 'TL.:<>: 'TL.ShowType s 'TL.:<>: 'TL.Text " is required, but you're asking for an optional field."
          'TL.:$$: 'TL.Text "  Try the required variation, e.g. (#.) instead of (#?)"
      )
  OptionalAttrType' all (_ ': as) s = OptionalAttrType' all as s
  OptionalAttrType' all '[] s =
    TL.TypeError
      ( 'TL.Text "Schema for attribute set does not declare a field named " 'TL.:<>: 'TL.ShowType s 'TL.:<>: 'TL.Text "."
          'TL.:$$: 'TL.Text "  Known attributes are " 'TL.:<>: 'TL.ShowType all
      )

-- | Like 'Proxy', but with an 'IsLabel' instance. For use with '(^#)'
data AttrLabel a = AttrLabel

instance (s ~ t) => IsLabel s (AttrLabel t) where
  fromLabel = AttrLabel

infixl 9 #.

infixl 9 >>.

type MonadEval m = (MonadIO m, MonadReader (Ptr EvalState) m)

-- | A combination of '>>=' and '#.'.
(>>.) :: (KnownSymbol s, AttrType as s ~ b, MonadEval m) => m (PSObject (Attrs' as w)) -> AttrLabel s -> m (PSObject b)
mas >>. p = mas >>= \as -> as #. p

-- | Attribute selector. @a #. #b@ is @a.b@ in Nix. Operates on attributes that are required (@_.@) in the schema, throwing an error if necessary.
(#.) :: (KnownSymbol s, AttrType as s ~ b, MonadEval m) => PSObject (Attrs' as w) -> AttrLabel s -> m (PSObject b)
as #. p = do
  evalState <- ask
  let name = T.pack (symbolVal p)
  v <- check as
  liftIO (getAttr evalState v (encodeUtf8 name)) >>= \case
    Nothing -> throwIO $ MissingAttribute (provenance as) name
    Just b -> pure PSObject {value = b, provenance = Attribute (provenance as) name}

-- | A combination of '>>=' and '#?'.
(>>?) :: (KnownSymbol s, OptionalAttrType as s ~ b, MonadEval m) => m (PSObject (Attrs' as w)) -> AttrLabel s -> m (Maybe (PSObject b))
mas >>? p = mas >>= \as -> as #? p

-- | Attribute selector. @a #? #b@ is @a.b@ in Nix, but handles the missing case without exception. Operates on attributes that are optional (@_?@) in the schema, throwing an error if necessary.
(#?) :: (KnownSymbol s, OptionalAttrType as s ~ b, MonadEval m) => PSObject (Attrs' as w) -> AttrLabel s -> m (Maybe (PSObject b))
as #? p = do
  evalState <- ask
  let name = T.pack (symbolVal p)
  v <- check as
  liftIO (getAttr evalState v (encodeUtf8 name))
    <&> fmap (\b -> PSObject {value = b, provenance = Attribute (provenance as) name})

-- | Retrieve an optional attribute but throw if it's missing.
--
-- It provides a decent error message with attrset provenance, but can't provide
-- extra context like you can when manually handling the @a '#?' b@ 'Nothing' case.
(#?!) :: (KnownSymbol s, OptionalAttrType as s ~ b, MonadEval m) => PSObject (Attrs' as w) -> AttrLabel s -> m (PSObject b)
as #?! p = do
  as #? p >>= \case
    Nothing -> throwIO $ MissingAttribute (provenance as) (T.pack (symbolVal p))
    Just x -> pure x

lookupDictBS :: MonadEval m => ByteString -> PSObject (Attrs' as w) -> m (Maybe (PSObject w))
lookupDictBS name as = do
  evalState <- ask
  v <- check as
  liftIO (getAttr evalState v name)
    <&> fmap (\b -> PSObject {value = b, provenance = Attribute (provenance as) (decodeUtf8With lenientDecode name)})

lookupDict :: MonadEval m => Text -> PSObject (Attrs' as w) -> m (Maybe (PSObject w))
lookupDict name as = do
  evalState <- ask
  v <- check as
  liftIO (getAttr evalState v (encodeUtf8 name))
    <&> fmap (\b -> PSObject {value = b, provenance = Attribute (provenance as) name})

-- | Like '#?!'. Throws an acceptable but not great error message.
requireDictBS :: MonadEval m => ByteString -> PSObject (Attrs' as w) -> m (PSObject w)
requireDictBS name as = do
  lookupDictBS name as >>= \case
    Nothing -> throwIO $ MissingAttribute (provenance as) (decodeUtf8With lenientDecode name)
    Just r -> pure r

-- | Like '#?!'. Throws an acceptable but not great error message.
requireDict :: MonadEval m => Text -> PSObject (Attrs' as w) -> m (PSObject w)
requireDict name as = do
  lookupDict name as >>= \case
    Nothing -> throwIO $ MissingAttribute (provenance as) name
    Just r -> pure r

dictionaryToMap :: MonadEval m => PSObject (Dictionary w) -> m (Map ByteString (PSObject w))
dictionaryToMap dict = do
  (liftIO . Expr.getAttrs =<< check dict)
    <&> M.mapWithKey
      ( \name b ->
          PSObject {value = b, provenance = Attribute (provenance dict) (decodeUtf8With lenientDecode name)}
      )

type family NixTypeForSchema s where
  NixTypeForSchema (Attrs' _ _) = NixAttrs
  NixTypeForSchema (_ ->. _) = NixFunction
  NixTypeForSchema NixString = NixString
  NixTypeForSchema StringWithoutContext = NixString
  NixTypeForSchema NixPath = NixPath
  NixTypeForSchema Bool = Bool
  NixTypeForSchema Int64 = Int64

class PossibleTypesForSchema s where
  typesForSchema :: Proxy s -> [RawValueType]
  default typesForSchema :: HasRawValueType (NixTypeForSchema s) => Proxy s -> [RawValueType]
  typesForSchema _ = [getRawValueType (Proxy @(NixTypeForSchema s))]

instance PossibleTypesForSchema (Attrs' as w)

instance PossibleTypesForSchema (a ->. b)

instance PossibleTypesForSchema NixString

instance PossibleTypesForSchema NixPath

instance PossibleTypesForSchema Bool

instance PossibleTypesForSchema Int64

instance
  (PossibleTypesForSchema a, PossibleTypesForSchema b) =>
  PossibleTypesForSchema (a |. b)
  where
  typesForSchema _ = typesForSchema (Proxy @a) <> typesForSchema (Proxy @b)

-- | Force and check type, then continue without backtracking
(|!) ::
  forall a b c m.
  ( CheckType (NixTypeForSchema a),
    MonadIO m,
    MonadEval m,
    PossibleTypesForSchema a,
    PossibleTypesForSchema b
  ) =>
  (PSObject a -> m c) ->
  (PSObject b -> m c) ->
  PSObject (a |. b) ->
  m c
f |! g = \ab -> do
  evalState <- ask
  t <- liftIO $ checkType @(NixTypeForSchema a) evalState (value ab)
  rawType <- liftIO $ rawValueType (value ab)
  let c = canonicalRawType rawType
      -- This call makes it O(n*n) because of the nested |! calls, but n is small.
      ts = typesForSchema (Proxy @(a |. b))
  when (c `notElem` ts) do
    throwIO $ TypeError (provenance ab) c ts
  case t of
    Just _abChecked -> f (ab {value = value ab})
    Nothing -> g (ab {value = value ab})

englishOr :: [Text] -> Text
englishOr [] = "impossible"
englishOr [a] = a
englishOr [y, z] = y <> " or " <> z
englishOr (a : as) = a <> ", " <> englishOr as

-- | Optional application.
($?) :: (MonadEval m, PossibleTypesForSchema a, PossibleTypesForSchema b) => PSObject (a ->? b) -> PSObject a -> m (PSObject b)
x $? a =
  pure x >>$? pure a

-- | Optional application. Like '$?' but takes care of monadic binding as a convenience.
(>>$?) :: (MonadEval m, PossibleTypesForSchema a, PossibleTypesForSchema b) => m (PSObject (a ->? b)) -> m (PSObject a) -> m (PSObject b)
x >>$? a =
  ( (\f -> a >>= (f .$))
      |! pure
  )
    =<< x

-- | Application. Like '$.' but takes care of monadic binding as a convenience.
(>>$.) :: (MonadEval m, PossibleTypesForSchema a, PossibleTypesForSchema b) => m (PSObject (a ->. b)) -> m (PSObject a) -> m (PSObject b)
f >>$. a = do
  f' <- f
  a' <- a
  f' .$ a'

-- | Parses an expression from string
exprWithBasePath ::
  forall schema m.
  (MonadEval m) =>
  -- | Expression text in the Nix language.
  Text ->
  -- | Base path for relative path references in the expression text.
  FilePath ->
  -- | A schema that the expression should satisfy.
  Proxy schema ->
  m (PSObject schema)
exprWithBasePath expr = exprWithBasePathBS (encodeUtf8 expr)

-- | Parses an expression from string
exprWithBasePathBS ::
  forall schema m.
  (MonadEval m) =>
  -- | Expression text in the Nix language.
  ByteString ->
  -- | Base path for relative path references in the expression text.
  FilePath ->
  -- | A schema that the expression should satisfy.
  Proxy schema ->
  m (PSObject schema)
exprWithBasePathBS expr path _ = do
  evalState <- ask
  v <- liftIO (valueFromExpressionString evalState expr (encodeUtf8 (toS path)))
  pure $ PSObject {provenance = Other "internal expression", value = v}

-- | Ignores string context.
getByteString_ ::
  (MonadEval m) =>
  PSObject NixString ->
  m ByteString
getByteString_ s = do
  check s >>= liftIO . Expr.getStringIgnoreContext

-- | Ignores string context.
getText_ ::
  (MonadEval m) =>
  PSObject NixString ->
  m Text
getText_ = validateE getByteString_ decodeUtf8' InvalidText

validate :: Monad m => (PSObject s -> m a) -> (Provenance -> a -> m b) -> PSObject s -> m b
validate basicParse validator o = do
  a <- basicParse o
  validator (provenance o) a

validateE :: MonadIO m => (PSObject s -> m a) -> (a -> Either e b) -> (Provenance -> e -> NixException) -> PSObject s -> m b
validateE basicParse validator thrower =
  validate basicParse \prov a ->
    case validator a of
      (Left e) -> throwIO (thrower prov e)
      (Right b) -> pure b

-- | Force a value and check against schema.
check ::
  forall schema m.
  ( CheckType (NixTypeForSchema schema),
    HasRawValueType (NixTypeForSchema schema),
    MonadEval m
  ) =>
  PSObject schema ->
  m (Value (NixTypeForSchema schema))
check pv = do
  evalState <- ask
  liftIO do
    checkType evalState (value pv) >>= \case
      Nothing -> do
        t <- rawValueType (value pv)
        throwIO $ TypeError (provenance pv) t [getRawValueType (Proxy @(NixTypeForSchema schema))]
      Just x -> pure x

-- TODO make this actually schema-based
toPSObject ::
  (MonadEval m, Expr.ToRawValue a) =>
  a ->
  m (PSObject (Expr.NixTypeFor a))
toPSObject a = do
  evalState <- ask
  v <- liftIO (toRawValue evalState a)
  pure (PSObject {provenance = Data, value = v})

uncheckedCast :: forall (a :: Type) (b :: Type). PSObject a -> PSObject b
uncheckedCast = coerce

-- | Schema-based parsing type class that constrains neither types nor schemas.
class FromPSObject schema a where
  -- | Parse an object assumed to be in schema @schema@ into a value of type @a@
  -- or throw a 'NixException'.
  fromPSObject :: MonadEval m => PSObject schema -> m a

instance FromPSObject StringWithoutContext ByteString where
  fromPSObject o = do
    v <- check o
    liftIO do
      c <- hasContext v
      when c do
        throwIO $ StringContextNotAllowed (provenance o)
    liftIO $ getStringIgnoreContext v

instance FromPSObject StringWithoutContext Text where
  fromPSObject = validateE fromPSObject decodeUtf8' InvalidText

instance FromPSObject StringWithoutContext [Char] where
  fromPSObject = fmap T.unpack . fromPSObject

instance FromPSObject Bool Bool where
  fromPSObject o = do
    v <- check o
    liftIO (Expr.getBool v)

basicAttrsWithProvenance :: Value NixAttrs -> Provenance -> PSObject (Attrs '[])
basicAttrsWithProvenance attrs p = PSObject {value = rtValue attrs, provenance = p}
