{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Hercules.CNix.Expr.RootedValue where

import Data.String (IsString (fromString))
import qualified Data.Text as T
import Hercules.CNix.Expr (CheckType, EvalState, NixAttrs, NixFunction, NixString, RawValue, Value, apply, checkType, getAttr, rawValueType, toRawValue, valueFromExpressionString)
import qualified Hercules.CNix.Expr as Expr
import Protolude hiding (TypeError, check, evalState)

data Provenance
  = File FilePath
  | Other Text
  | Data
  | Attribute Provenance Text
  | -- | ListIndex Provenance Int TBD
    Application Provenance Provenance
  deriving (Show, Eq, Ord)

data NixException
  = MissingAttribute Provenance Text
  | TypeError Provenance Text
  deriving (Show, Exception, Eq)

-- data RootedValue = RootedValue
--   { provenance :: Provenance,
--     value :: RawValue
--   }

-- | Alternative schema. The value only needs to satisfy one subschema.
data a ^| b

-- | Function schema.
data a ^-> b

-- | Optional function. If the value is not a function, use it as the result.
type a ^->? b = (a ^-> b) ^| b

-- | Attribute set schema with known attributes and wildcard type for remaining attributes.
data Attrs' (as :: [Attr]) w

-- | Attribute set schema with known attributes only
type Attrs as = Attrs' as Void

-- | Attribute set functioning as a "dictionary" from string keys to a certain type.
type Dict = Attrs' '[]

data Attr
  = -- | Required attribute
    Symbol :. Type
  | -- | Optional attribute
    Symbol :? Type

infix 1 :.

infix 1 :?

newtype SValue a = SValue {runSValue :: Ptr EvalState -> IO (Provenance, RawValue)}

type HerculesCISchema = Attrs '["onPush" ':? Dict OnPush]

type OnPush =
  Attrs
    '[ "extraInputs" ':? Dict InputDecl,
       "outputs" ':. Dict (Dict Value) ^->? Dict Value
     ]

type InputDecl = Attrs '[]

(^$) :: SValue (a ^-> b) -> SValue a -> SValue b
SValue f ^$ SValue a = SValue $ \evalState -> do
  (fpr, f') <- f evalState
  (apr, a') <- a evalState
  (Application fpr apr,) <$> f' `apply` a'

type family AttrType as s where
  AttrType ((s ':. t) ': as) s = t
  AttrType (_ ': as) s = AttrType as s

-- | Like 'Proxy', but with an 'IsLabel' instance. For use with '(^#)'
data AttrLabel a = AttrLabel

instance (s ~ t) => IsLabel s (AttrLabel t) where
  fromLabel = AttrLabel

infixl 9 ^#

(^#) :: (KnownSymbol s, AttrType as s ~ b) => SValue (Attrs' as w) -> AttrLabel s -> SValue b
SValue as ^# p = SValue $ \evalState -> do
  (pas, as') <- as evalState
  as'' <-
    checkType evalState as' >>= \case
      Nothing -> do
        t <- rawValueType as'
        throwIO $ TypeError pas $ "Expected an attribute set, but got " <> show t
      Just x -> pure x
  let name = T.pack (symbolVal p)
  getAttr evalState as'' (encodeUtf8 name) >>= \case
    Nothing -> throwIO $ MissingAttribute pas name
    Just x -> pure (Attribute pas name, x)

type family NixTypeForSchema s where
  NixTypeForSchema (Attrs' _ _) = NixAttrs
  NixTypeForSchema (_ ^-> _) = NixFunction

-- class Checkable s where
--   type NixTypeForSchema s :: *
--   forceAndCheck :: Ptr EvalState -> SValue s -> IO (Provenance, Maybe (Value (NixTypeForSchema s)))

-- | Force and check type, then continue without backtracking
(^|!) :: forall a b c. (CheckType (NixTypeForSchema a)) => (SValue a -> SValue c) -> (SValue b -> SValue c) -> SValue (a ^| b) -> SValue c
f ^|! g = \(SValue fab) ->
  SValue \evalState -> do
    (abProv, ab) <- fab evalState
    t <- checkType @(NixTypeForSchema a) evalState ab
    case t of
      Just _abChecked -> runSValue (f (SValue \_evalState -> pure (abProv, ab))) evalState
      Nothing -> runSValue (g (SValue \_evalState -> pure (abProv, ab))) evalState

(^$?) :: SValue (a ^->? b) -> SValue a -> SValue b
x ^$? a = ((^$ a) ^|! identity) x

instance (nixString ~ NixString) => IsString (SValue nixString) where
  fromString s = SValue \evalState -> (Data,) <$> toRawValue evalState (fromString s :: Text)

run :: (MonadReader (Ptr EvalState) m, MonadIO m) => SValue a -> m (Provenance, RawValue)
run m = do
  evalState <- ask
  liftIO (runSValue m evalState)

evalWithBasePath :: Text -> FilePath -> Proxy schema -> SValue schema
evalWithBasePath expr path _ = SValue \evalState ->
  (Other "internal expression",)
    <$> valueFromExpressionString evalState (encodeUtf8 expr) (encodeUtf8 (toS path))

memo :: (MonadReader (Ptr EvalState) m, MonadIO m) => SValue a -> m (SValue a)
memo s = do
  (p, v) <- run s
  pure (SValue \_evalState -> pure (p, v))

getStringIgnoreContext ::
  (MonadReader (Ptr EvalState) m, MonadIO m) =>
  SValue NixString ->
  m ByteString
getStringIgnoreContext s = do
  (p, v) <- run s
  evalState <- ask
  liftIO do
    sv <-
      checkType evalState v >>= \case
        Nothing -> do
          t <- rawValueType v
          throwIO $ TypeError p $ "Expected a string, but got " <> show t
        Just x -> pure x
    Expr.getStringIgnoreContext sv
