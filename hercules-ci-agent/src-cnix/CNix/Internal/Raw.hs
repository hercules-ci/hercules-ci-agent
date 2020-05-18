{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module CNix.Internal.Raw where

import CNix.Internal.Context
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import Protolude hiding (evalState)
import Prelude ()

C.context context

C.include "<nix/config.h>"

C.include "<nix/eval.hh>"

C.include "<nix/eval-inline.hh>"

C.include "aliases.h"

C.include "<gc/gc.h>"

C.include "<gc/gc_cpp.h>"

C.include "<gc/gc_allocator.h>"

C.using "namespace nix"

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
      switch ($(Value* v)->type) {
        case tInt:         return 1;
        case tBool:        return 2;
        case tString:      return 3;
        case tPath:        return 4;
        case tNull:        return 5;
        case tAttrs:       return 6;
        case tList1:       return 7;
        case tList2:       return 8;
        case tListN:       return 9;
        case tThunk:       return 10;
        case tApp:         return 11;
        case tLambda:      return 12;
        case tBlackhole:   return 13;
        case tPrimOp:      return 14;
        case tPrimOpApp:   return 15;
        case tExternal:    return 16;
        case tFloat:       return 17;
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
    f 8 = List
    f 9 = List
    f 10 = Thunk
    f 11 = App
    f 12 = Lambda
    f 13 = Blackhole
    f 14 = PrimOp
    f 15 = PrimOpApp
    f 16 = External
    f 17 = Float
    f _ = Other

forceValue :: Exception a => Ptr EvalState -> RawValue -> IO (Either a ())
forceValue evalState (RawValue v) =
  try
    [C.catchBlock|  {
      Value *v = $(Value *v);
      if (v == NULL) throw std::invalid_argument("forceValue value must be non-null");
      $(EvalState *evalState)->forceValue(*v);
    }|]
