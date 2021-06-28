{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.CNix.Expr
  ( module Hercules.CNix.Expr,
    RawValue,
    rawValueType,
    module Hercules.CNix.Store,
    module Hercules.CNix.Expr.Typed,
    type EvalState,
  )
where

-- TODO: No more Ptr EvalState
-- TODO: No more NixStore when EvalState is already there
-- TODO: Map Nix-specific C++ exceptions to a CNix exception type

import Conduit
import qualified Data.Map as M
import Foreign (nullPtr)
import qualified Foreign.C.String
import Hercules.CNix.Encapsulation (moveToForeignPtrWrapper)
import Hercules.CNix.Expr.Context
import Hercules.CNix.Expr.Raw
import Hercules.CNix.Expr.Typed
import Hercules.CNix.Store
import Hercules.CNix.Store.Context
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import Protolude hiding (evalState, throwIO)

C.context (Hercules.CNix.Store.Context.context <> Hercules.CNix.Expr.Context.evalContext)

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

C.include "<nix-compat.hh>"

C.include "hercules-ci-cnix/expr.hxx"

C.include "<gc/gc.h>"

C.include "<gc/gc_cpp.h>"

C.include "<gc/gc_allocator.h>"

C.using "namespace nix"

C.verbatim "\nGC_API void GC_CALL GC_throw_bad_alloc() { throw std::bad_alloc(); }\n"

init :: IO ()
init =
  void
    [C.throwBlock| void {
      nix::initNix();
      nix::initGC();
    } |]

setTalkative :: IO ()
setTalkative =
  [C.throwBlock| void {
    nix::verbosity = nix::lvlTalkative;
  } |]

setDebug :: IO ()
setDebug =
  [C.throwBlock| void {
    nix::verbosity = nix::lvlVomit;
  } |]

setGlobalOption :: Text -> Text -> IO ()
setGlobalOption opt value = do
  let optionStr = encodeUtf8 opt
      valueStr = encodeUtf8 value
  [C.throwBlock| void {
    globalConfig.set($bs-cstr:optionStr, $bs-cstr:valueStr);
  }|]

setOption :: Text -> Text -> IO ()
setOption opt value = do
  let optionStr = encodeUtf8 opt
      valueStr = encodeUtf8 value
  [C.throwBlock| void {
    settings.set($bs-cstr:optionStr, $bs-cstr:valueStr);
  }|]

logInfo :: Text -> IO ()
logInfo t = do
  let bstr = encodeUtf8 t
  [C.throwBlock| void {
    printInfo($bs-cstr:bstr);
  }|]

withEvalState ::
  Store ->
  (Ptr EvalState -> IO a) ->
  IO a
withEvalState (Store store) =
  bracket
    ( liftIO
        [C.throwBlock| EvalState* {
          Strings searchPaths;
          return new EvalState(searchPaths, *$(refStore* store));
        } |]
    )
    (\x -> liftIO [C.throwBlock| void { delete $(EvalState* x); } |])

withEvalStateConduit ::
  MonadResource m =>
  Store ->
  (Ptr EvalState -> ConduitT i o m r) ->
  ConduitT i o m r
withEvalStateConduit (Store store) =
  bracketP
    ( liftIO
        [C.throwBlock| EvalState* {
          Strings searchPaths;
          return new EvalState(searchPaths, *$(refStore* store));
        } |]
    )
    (\x -> liftIO [C.throwBlock| void { delete $(EvalState* x); } |])

evalFile :: Ptr EvalState -> FilePath -> IO RawValue
evalFile evalState filename = do
  filename' <- Foreign.C.String.newCString filename
  mkRawValue
    =<< [C.throwBlock| Value* {
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
    $(Strings *ss)->push_back(std::string($bs-ptr:s, $bs-len:s));
  }|]

evalArgs :: Ptr EvalState -> [ByteString] -> IO (Value NixAttrs)
evalArgs evalState args = do
  argsStrings <- newStrings
  forM_ args $ appendString argsStrings
  fmap unsafeAssertType . mkRawValue
    =<< [C.throwBlock| Value * {
      Strings *args = $(Strings *argsStrings);
      struct MixEvalArgs evalArgs;
      Bindings *autoArgs;
      EvalState &state = *$(EvalState *evalState);

      evalArgs.parseCmdline(*args);
      autoArgs = evalArgs.getAutoArgs(state);
      if (!autoArgs) {
        throw nix::Error("Could not evaluate automatic arguments");
      }
      Value *r = new (NoGC) Value ();
#ifdef NIX_2_4
      r->mkAttrs(autoArgs);
#else
      r->type = tAttrs;
      r->attrs = autoArgs;
#endif
      return r;
    }|]

autoCallFunction :: Ptr EvalState -> RawValue -> Value NixAttrs -> IO RawValue
autoCallFunction evalState (RawValue fun) (Value (RawValue autoArgs)) =
  mkRawValue
    =<< [C.throwBlock| Value* {
          Value result;
          $(EvalState *evalState)->autoCallFunction(
                  *$(Value *autoArgs)->attrs,
                  *$(Value *fun),
                  result);
          return new (NoGC) Value (result);
        }|]

isDerivation :: Ptr EvalState -> RawValue -> IO Bool
isDerivation evalState (RawValue v) =
  (0 /=)
    <$> [C.throwBlock| int {
          if ($(Value *v) == NULL) { throw std::invalid_argument("forceValue value must be non-null"); }
          $(EvalState *evalState)->forceValue(*$(Value *v));
          return $(EvalState *evalState)->isDerivation(*$(Value *v));
        }|]

isFunctor :: Ptr EvalState -> RawValue -> IO Bool
isFunctor evalState (RawValue v) =
  (0 /=)
    <$> [C.throwBlock| int {
          if ($(Value *v) == NULL) { throw std::invalid_argument("forceValue value must be non-null"); }
          return $(EvalState *evalState)->isFunctor(*$(Value *v));
        }|]

getRecurseForDerivations :: Ptr EvalState -> Value NixAttrs -> IO Bool
getRecurseForDerivations evalState (Value (RawValue v)) =
  (0 /=)
    <$> [C.throwBlock| int {
          Value *v = $(Value *v);
          EvalState &evalState = *$(EvalState *evalState);
#ifdef NIX_2_4
          Bindings::iterator iter = v->attrs->find(evalState.sRecurseForDerivations);
          if (iter == v->attrs->end()) {
            return 0;
          } else {
            // Previously this bool was unpacked manually and included a special
            // case to return true when it is not a bool. That logic was added
            // because an empty attrset was found here, observed in
            // nixpkgs master 67e2de195a4aa0a50ffb1e1ba0b4fb531dca67dc
            return evalState.forceBool(*iter->value, *iter->pos);
          }
#else
          Symbol rfd = evalState.symbols.create("recurseForDerivations");
          Bindings::iterator iter = v->attrs->find(rfd);
          if (iter == v->attrs->end()) {
            return 0;
          } else {
            evalState.forceValue(*iter->value);
            if (iter->value->type == ValueType::tBool) {
              return iter->value->boolean ? 1 : 0;
            } else {
              // They can be empty attrsets???
              // Observed in nixpkgs master 67e2de195a4aa0a50ffb1e1ba0b4fb531dca67dc
              return 1;
            }
          }
#endif
        } |]

getAttr :: Ptr EvalState -> Value NixAttrs -> ByteString -> IO (Maybe RawValue)
getAttr evalState (Value (RawValue v)) k =
  mkNullableRawValue
    =<< [C.throwBlock| Value *{
      Value &v = *$(Value *v);
      EvalState &evalState = *$(EvalState *evalState);
      Symbol k = evalState.symbols.create($bs-cstr:k);
      Bindings::iterator iter = v.attrs->find(k);
      if (iter == v.attrs->end()) {
        return nullptr;
      } else {
        return iter->value;
      }
    }|]

-- | Converts 'nullPtr' to 'Nothing'; actual values to @Just (a :: 'RawValue')@
mkNullableRawValue :: Ptr Value' -> IO (Maybe RawValue)
mkNullableRawValue p | p == nullPtr = pure Nothing
mkNullableRawValue p = Just <$> mkRawValue p

getAttrs :: Value NixAttrs -> IO (Map ByteString RawValue)
getAttrs (Value (RawValue v)) = do
  begin <- [C.exp| Attr *{ $(Value *v)->attrs->begin() }|]
  end <- [C.exp| Attr *{ $(Value *v)->attrs->end() }|]
  let gather :: Map ByteString RawValue -> Ptr Attr' -> IO (Map ByteString RawValue)
      gather acc i | i == end = pure acc
      gather acc i = do
        name <- unsafeMallocBS [C.exp| const char *{ strdup(static_cast<std::string>($(Attr *i)->name).c_str()) } |]
        value <- mkRawValue =<< [C.exp| Value *{ new (NoGC) Value(*$(Attr *i)->value) } |]
        let acc' = M.insert name value acc
        seq acc' pass
        gather acc' =<< [C.exp| Attr *{ &$(Attr *i)[1] }|]
  gather mempty begin

getDrvFile :: MonadIO m => Ptr EvalState -> RawValue -> m StorePath
getDrvFile evalState (RawValue v) = liftIO do
  moveToForeignPtrWrapper
    =<< [C.throwBlock| nix::StorePath *{
      EvalState &state = *$(EvalState *evalState);
      auto drvInfo = getDerivation(state, *$(Value *v), false);
      if (!drvInfo)
        throw EvalError("Not a valid derivation");

      std::string drvPath = drvInfo->queryDrvPath();
      StorePath storePath = parseStorePath(*state.store, drvPath);

      // write it (?)
      auto drv = state.store->derivationFromPath(printPath23(*state.store, storePath));

      return new StorePath(storePath);
    }|]

getAttrBool :: Ptr EvalState -> Value NixAttrs -> ByteString -> IO (Either SomeException (Maybe Bool))
getAttrBool evalState attrset attrName = do
  attrMaybe <- getAttr evalState attrset attrName
  attrMaybe & maybe (pure (Right Nothing)) \attr -> do
    match evalState attr >>= \case
      Left e -> do
        pure $ Left e
      Right (IsBool r) -> do
        b <- getBool r
        pure $ Right (Just b)
      Right _ -> do
        pure $ Right Nothing

getList :: Value NixList -> IO [RawValue]
getList (Value (RawValue nixList)) = do
  len <- [C.exp| int { $(Value *nixList)->listSize() }|]
  let getElem i = mkRawValue =<< [C.exp| Value * { $(Value *nixList)->listElems()[$(int i)] }|]
  for [0 .. (len - 1)] \i -> do
    getElem i

getAttrList :: Ptr EvalState -> Value NixAttrs -> ByteString -> IO (Either SomeException (Maybe [RawValue]))
getAttrList evalState attrset attrName = do
  attrMaybe <- getAttr evalState attrset attrName
  attrMaybe & maybe (pure (Right Nothing)) \attr -> do
    match evalState attr >>= \case
      Left e -> do
        pure $ Left e
      Right (IsList r) -> do
        b <- getList r
        pure $ Right (Just b)
      Right _ -> do
        pure $ Right Nothing
