{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.CNix.Expr.Typed where

import Control.Exception (throwIO)
import Hercules.CNix.Expr.Context
import Hercules.CNix.Expr.Raw
import qualified Language.C.Inline.Cpp as C
import Protolude hiding
  ( evalState,
    throwIO,
  )
import Prelude (userError)

C.context context

C.include "<stdio.h>"

C.include "<cstring>"

C.include "<math.h>"

C.include "<nix/config.h>"

C.include "<nix/shared.hh>"

C.include "<nix/eval.hh>"

C.include "<nix/eval-inline.hh>"

C.include "<nix/store-api.hh>"

C.include "<nix/common-eval-args.hh>"

C.include "<nix/get-drvs.hh>"

C.include "<nix/derivations.hh>"

C.include "<nix/affinity.hh>"

C.include "<nix/globals.hh>"

C.include "hercules-ci-cnix/expr.hxx"

C.include "<gc/gc.h>"

C.include "<gc/gc_cpp.h>"

C.include "<gc/gc_allocator.h>"

C.using "namespace nix"

-- | Runtime-Typed Value. This implies that it has been forced,
-- because otherwise the type would not be known.
--
-- This is distinct from Nix, which calls its objects @Value@ regardless if
-- they're thunks.
newtype Value a = Value {rtValue :: RawValue}

type NixInt = Int64

data NixFloat

data NixString

data NixPath

data NixAttrs

data NixFunction

data NixList

data NixPrimOp

data NixPrimOpApp

data NixExternal

-- TODO: actually encapsulate the constructor
unsafeAssertType :: RawValue -> Value a
unsafeAssertType = Value

-- This is useful because you regain exhaustiveness checking.
-- Otherwise a bunch of downcast functions might do.
data Match
  = IsInt (Value NixInt)
  | IsBool (Value Bool)
  | IsString (Value NixString)
  | IsPath (Value NixPath)
  | IsNull (Value ())
  | IsAttrs (Value NixAttrs)
  | IsList (Value NixList)
  | IsFunction (Value NixFunction)
  | IsExternal (Value NixExternal)
  | IsFloat (Value NixFloat)

-- FIXME: errors don't provide any clue here
match :: Ptr EvalState -> RawValue -> IO (Either SomeException Match)
match es v =
  forceValue es v >>= \case
    Left e -> pure (Left e)
    Right _ ->
      rawValueType v <&> \case
        Int -> pure $ IsInt $ unsafeAssertType v
        Bool -> pure $ IsBool $ unsafeAssertType v
        String -> pure $ IsString $ unsafeAssertType v
        Path -> pure $ IsPath $ unsafeAssertType v
        Null -> pure $ IsNull $ unsafeAssertType v
        Attrs -> pure $ IsAttrs $ unsafeAssertType v
        List -> pure $ IsList $ unsafeAssertType v
        Thunk -> Left $ SomeException $ userError "Could not force Nix thunk" -- FIXME: custom exception?
        App -> Left $ SomeException $ userError "Could not force Nix thunk (App)"
        Blackhole ->
          Left $ SomeException $ userError "Could not force Nix thunk (Blackhole)"
        Lambda -> pure $ IsFunction $ unsafeAssertType v
        PrimOp -> pure $ IsFunction $ unsafeAssertType v
        PrimOpApp -> pure $ IsFunction $ unsafeAssertType v
        External -> pure $ IsExternal $ unsafeAssertType v
        Float -> pure $ IsFloat $ unsafeAssertType v
        Other ->
          Left $ SomeException $ userError "Unknown runtime type in Nix value"

match' :: Ptr EvalState -> RawValue -> IO Match
match' es v = match es v >>= \case Left e -> throwIO e; Right a -> pure a

getBool :: Value Bool -> IO Bool
getBool (Value (RawValue v)) =
  (0 /=)
    <$> [C.exp| int { $(Value *v)->boolean ? 1 : 0 }|]

-- NOT coerceToString
getStringIgnoreContext :: Value NixString -> IO ByteString
getStringIgnoreContext (Value (RawValue v)) =
  unsafeMallocBS
    [C.exp| const char *{
    strdup($(Value *v)->string.s)
  }|]

class CheckType a where
  checkType :: Ptr EvalState -> RawValue -> IO (Maybe (Value a))

instance CheckType Int64 where
  checkType es v = match' es v <&> \case IsInt x -> pure x; _ -> Nothing

instance CheckType Bool where
  checkType es v = match' es v <&> \case IsBool x -> pure x; _ -> Nothing

instance CheckType NixString where
  checkType es v = match' es v <&> \case IsString x -> pure x; _ -> Nothing

instance CheckType NixPath where
  checkType es v = match' es v <&> \case IsPath x -> pure x; _ -> Nothing

instance CheckType () where
  checkType es v = match' es v <&> \case IsNull x -> pure x; _ -> Nothing

instance CheckType NixAttrs where
  checkType es v = match' es v <&> \case IsAttrs x -> pure x; _ -> Nothing

instance CheckType NixList where
  checkType es v = match' es v <&> \case IsList x -> pure x; _ -> Nothing

instance CheckType NixFunction where
  checkType es v = match' es v <&> \case IsFunction f -> pure f; _ -> Nothing

instance CheckType NixExternal where
  checkType es v = match' es v <&> \case IsExternal x -> pure x; _ -> Nothing

instance CheckType NixFloat where
  checkType es v = match' es v <&> \case IsFloat f -> pure f; _ -> Nothing

assertType :: (HasCallStack, MonadIO m, CheckType t) => Ptr EvalState -> RawValue -> m (Value t)
assertType es v = do
  liftIO (checkType es v) >>= \case
    Nothing -> withFrozenCallStack (panic "Unexpected type")
    Just x -> pure x

class HasRawValueType s where
  getRawValueType :: Proxy s -> RawValueType

instance HasRawValueType NixString where
  getRawValueType _ = String

instance HasRawValueType Int64 where
  getRawValueType _ = Int

instance HasRawValueType Bool where
  getRawValueType _ = Bool

instance HasRawValueType NixFloat where
  getRawValueType _ = Float

instance HasRawValueType NixPath where
  getRawValueType _ = Path

instance HasRawValueType NixAttrs where
  getRawValueType _ = Attrs

instance HasRawValueType NixFunction where
  getRawValueType _ = Lambda

instance HasRawValueType NixList where
  getRawValueType _ = List
