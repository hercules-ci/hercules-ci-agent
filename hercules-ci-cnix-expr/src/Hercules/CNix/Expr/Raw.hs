{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.CNix.Expr.Raw where

import Hercules.CNix.Expr.Context
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import Protolude hiding (evalState)
import Prelude ()

C.context context

C.include "<nix/config.h>"

C.include "<nix/eval.hh>"

C.include "<nix/eval-inline.hh>"

C.include "<hercules-ci-cnix/expr.hxx>"

C.include "<gc/gc.h>"

C.include "<gc/gc_cpp.h>"

C.include "<gc/gc_allocator.h>"

C.using "namespace nix"

-- | A heap object.
--
-- Nix doesn't store all its objects on the heap, but we do.
--
-- Also, Nix calls them @Value@s but it includes thunks, which are not values
-- and some may never produce values, such as @throw "msg"@.
newtype RawValue = RawValue (Ptr Value')

-- | Takes ownership of the value.
mkRawValue :: Ptr Value' -> IO RawValue
mkRawValue p = pure $ RawValue p

-- | Similar to Nix's Value->type but conflates the List variations
data RawValueType
  = Int
  | Bool
  | String
  | Path
  | Null
  | Attrs
  | List
  | Thunk
  | App
  | Lambda
  | Blackhole
  | PrimOp
  | PrimOpApp
  | External
  | Float
  | Other
  deriving (Generic, Show, Eq, Ord)

-- | You may need to 'forceValue' first.
rawValueType :: RawValue -> IO RawValueType
rawValueType (RawValue v) =
  f
    <$> [C.block| int {
      switch ($(Value* v)->type()) {
        case nInt:         return 1;
        case nBool:        return 2;
        case nString:      return 3;
        case nPath:        return 4;
        case nNull:        return 5;
        case nAttrs:       return 6;
        case nList:        return 7;
        case nFunction:    return 8;
        case nExternal:    return 9;
        case nFloat:       return 10;
        case nThunk:       return 11;
        default: return 0;
      }
    }|]
  where
    f 1 = Int
    f 2 = Bool
    f 3 = String
    f 4 = Path
    f 5 = Null
    f 6 = Attrs
    f 7 = List
    f 8 = Lambda
    f 9 = External
    f 10 = Float
    f 11 = Thunk
    f _ = Other

forceValue :: Exception a => Ptr EvalState -> RawValue -> IO (Either a ())
forceValue evalState (RawValue v) =
  try
    [C.catchBlock|  {
      Value *v = $(Value *v);
      if (v == NULL) throw std::invalid_argument("forceValue value must be non-null");
      $(EvalState *evalState)->forceValue(*v, nix::noPos);
    }|]

-- | Brings RawValueType closer to the 2.4 ValueType.
--
-- This function won't be necessary when support for 2.3 is dropped and we
-- switch entirely to the Haskell equivalent of C++ ValueType.
canonicalRawType :: RawValueType -> RawValueType
canonicalRawType = \case
  App -> Thunk
  Blackhole -> Thunk
  PrimOp -> Lambda
  PrimOpApp -> Lambda
  x -> x
