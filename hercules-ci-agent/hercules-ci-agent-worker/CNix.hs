{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module CNix
  ( module CNix

  , RawValue
  , rawValueType

  , module CNix.Internal.Store
  , module CNix.Internal.Typed
  
  , type NixStore
  , type EvalState
  , type Ref
  ) where

-- TODO: No more Ptr EvalState
-- TODO: No more NixStore when EvalState is already there
-- TODO: Map Nix-specific C++ exceptions to a CNix exception type

import           Protolude hiding (evalState, throwIO)

import           Conduit
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Foreign.C.String
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import           CNix.Internal.Context
import           CNix.Internal.Raw
import           CNix.Internal.Store
import           CNix.Internal.Typed

newtype Bindings = Bindings (Ptr Bindings')

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

C.verbatim "\nGC_API void GC_CALL GC_throw_bad_alloc() { throw std::bad_alloc(); }\n"


init :: IO ()
init =
  void [C.throwBlock| void {
      nix::initNix();
      nix::initGC();
    } |]

setGlobalOption :: Text -> Text -> IO Bool
setGlobalOption opt value = do
  let optionStr = encodeUtf8 opt
      valueStr = encodeUtf8 value
  (== 0) <$> [C.throwBlock| int {
    globalConfig.set($bs-cstr:optionStr, $bs-cstr:valueStr);
  }|]

mkBindings :: Ptr Bindings' -> IO Bindings
mkBindings p = pure $ Bindings p

getBS :: MonadIO m => IO Foreign.C.String.CString -> m ByteString
getBS m = liftIO (BS.packCString =<< m)

withEvalState
  :: MonadResource m
  => Ptr (Ref NixStore)
  -> (Ptr EvalState -> ConduitT i o m r)
  -> ConduitT i o m r
withEvalState store = bracketP (liftIO $ [C.throwBlock| EvalState* {
      Strings searchPaths;
      return new EvalState(searchPaths, *$(refStore* store));
    } |])
    (\x -> liftIO $ [C.throwBlock| void { delete $(EvalState* x); } |])

evalFile :: Ptr EvalState -> FilePath -> IO RawValue
evalFile evalState filename = do
  filename' <- Foreign.C.String.newCString filename
  mkRawValue =<< [C.throwBlock| Value* {
    Value value;
    $(EvalState *evalState)->evalFile($(const char *filename'), value);
    return new (NoGC) Value(value);
  }|]

-- leaks
newStrings :: IO (Ptr Strings)
newStrings = [C.exp| Strings* { new (NoGC) Strings() }|]

appendString :: Ptr Strings -> ByteString -> IO ()
appendString ss s =
  [C.block| void {
    $(Strings *ss)->push_back($bs-ptr:s);
  }|]

evalArgs :: Ptr EvalState -> [ByteString] -> IO Bindings
evalArgs evalState args = do
  argsStrings <- newStrings
  forM_ args $ appendString argsStrings

  mkBindings =<< [C.throwBlock| Bindings * {
    Strings *args = $(Strings *argsStrings);
    struct MixEvalArgs evalArgs;
    Bindings *autoArgs;
    EvalState &state = *$(EvalState *evalState);

    evalArgs.parseCmdline(*args);
    autoArgs = evalArgs.getAutoArgs(state);
    if (autoArgs == NULL) {
      //cerr << "Could not evaluate automatic arguments" << endl;
      abort(); // FIXME
    }
    return autoArgs;

    // catch (EvalError & e) {
    // catch (Error & e) {
    // catch (std::exception & e) {

  }|]

autoCallFunction :: Ptr EvalState -> RawValue -> Bindings -> IO RawValue
autoCallFunction evalState (RawValue fun) (Bindings autoArgs) =
  mkRawValue =<< [C.throwBlock| Value* {
    Value result;
    $(EvalState *evalState)->autoCallFunction(
            *$(Bindings *autoArgs),
            *$(Value *fun),
            result);
    return new (NoGC) Value (result);
  }|]

isDerivation :: Ptr EvalState -> RawValue -> IO Bool
isDerivation evalState (RawValue v) = (0 /=) <$> [C.throwBlock| int {
    if ($(Value *v) == NULL) { throw std::invalid_argument("forceValue value must be non-null"); }
    return $(EvalState *evalState)->isDerivation(*$(Value *v));
  }|]

isFunctor :: Ptr EvalState -> RawValue -> IO Bool
isFunctor evalState (RawValue v) = (0 /=) <$> [C.throwBlock| int {
    if ($(Value *v) == NULL) { throw std::invalid_argument("forceValue value must be non-null"); }
    return $(EvalState *evalState)->isFunctor(*$(Value *v));
  }|]

getRecurseForDerivations :: Ptr EvalState -> Value NixAttrs -> IO Bool
getRecurseForDerivations evalState (Value (RawValue v)) = (0 /=) <$> [C.throwBlock| int {
    Value *v = $(Value *v);
    EvalState *evalState = $(EvalState *evalState);
    Symbol rfd = evalState->symbols.create("recurseForDerivations");
    Bindings::iterator iter = v->attrs->find(rfd);
    if (iter == v->attrs->end()) {
      return 0;
    } else {
      evalState->forceValue(*iter->value);
      if (iter->value->type == ValueType::tBool) {
        return iter->value->boolean ? 1 : 0;
      } else {
        // They can be empty attrsets???
        // Observed in nixpkgs master 67e2de195a4aa0a50ffb1e1ba0b4fb531dca67dc
        return 1;
      }
    }
  } |]

getAttrBindings :: Value NixAttrs -> IO Bindings
getAttrBindings (Value (RawValue v)) = mkBindings =<< [C.exp| Bindings *{ $(Value *v)->attrs } |]

getAttrs :: Value NixAttrs -> IO (Map ByteString RawValue)
getAttrs (Value (RawValue v)) = do
  begin <- [C.exp| Attr *{ $(Value *v)->attrs->begin() }|]
  end <- [C.exp| Attr *{ $(Value *v)->attrs->end() }|]
  let
    gather :: Map ByteString RawValue -> Ptr Attr' -> IO (Map ByteString RawValue)
    gather acc i | i == end  = pure acc
    gather acc i | otherwise = do
                    name <- unsafeMallocBS [C.exp| const char *{ strdup(static_cast<std::string>($(Attr *i)->name).c_str()) } |]
                    value <- mkRawValue =<< [C.exp| Value *{ new (NoGC) Value(*$(Attr *i)->value) } |]
                    let acc' = M.insert name value acc
                    seq acc' pass
                    gather acc' =<< [C.exp| Attr *{ &$(Attr *i)[1] }|]

  gather mempty begin

getDrvFile :: MonadIO m => Ptr EvalState -> RawValue -> m ByteString
getDrvFile evalState (RawValue v) =
  unsafeMallocBS [C.throwBlock| const char *{
    EvalState &state = *$(EvalState *evalState);
    auto drvInfo = getDerivation(state, *$(Value *v), false);
    if (!drvInfo)
      throw EvalError("Not a valid derivation");

    std::string drvPath = drvInfo->queryDrvPath();

    // write it (?)
    auto drv = state.store->derivationFromPath(drvPath);

    return strdup(drvPath.c_str());
  }|]
