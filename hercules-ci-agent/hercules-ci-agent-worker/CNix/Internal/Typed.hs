
module CNix.Internal.Typed where

import           Prelude                        ( userError )
import           Protolude               hiding ( evalState
                                                , throwIO
                                                )

import           CNix.Internal.Context
import           CNix.Internal.Raw

-- | Runtime-Typed Value. This implies that it has been forced,
-- because otherwise the type would not be known.
newtype Value a = Value { rtValue :: RawValue }

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
match es v =
  forceValue es v >>= \case
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
