{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module CNix.Internal.Typed where

import CNix.Internal.Context
import CNix.Internal.Raw
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

C.include "aliases.h"

C.include "<gc/gc.h>"

C.include "<gc/gc_cpp.h>"

C.include "<gc/gc_allocator.h>"

C.using "namespace nix"

-- | Runtime-Typed Value. This implies that it has been forced,
-- because otherwise the type would not be known.
newtype Value a = Value {rtValue :: RawValue}

data NixInt

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
match es v = forceValue es v >>= \case
  Left e -> pure (Left e)
  Right _ -> rawValueType v <&> \case
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

getBool :: Value Bool -> IO Bool
getBool (Value (RawValue v)) =
  (0 /=)
    <$> [C.exp| int { $(Value *v)->boolean ? 1 : 0 }|]
