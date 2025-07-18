{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- redundant-constraints: False positive in default signature for `toRawValue`
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

#ifdef __GHCIDE__
# define NIX_IS_AT_LEAST(mm,m,p) 1
#endif

module Hercules.CNix.Expr
  ( init,
    setTalkative,
    setDebug,
    setGlobalOption,
    setOption,
    setExtraStackOverflowHandlerToSleep,
    initThread,
    allowThreads,
    runGcRegisteredThread,
    logInfo,
    withEvalState,
    withEvalStateConduit,
    addAllowedPath,
    addInternalAllowedPaths,
    evalFile,
    newStrings,
    appendString,
    autoCallFunction,
    isDerivation,
    isFunctor,
    getRecurseForDerivations,
    getAttr,
    mkNullableRawValue,
    getAttrs,
    getDrvFile,
    getAttrBool,
    getList,
    getAttrList,
    valueFromExpressionString,
    callFunction,
    apply,
    mkPath,
    getFlakeFromFlakeRef,
    getLocalFlake,
    getFlakeFromGit,
    getFlakeFromArchiveUrl,
    ToRawValue(..),
    ToValue(..),
    FromValue(..),
    ViaJSON(..),

    -- * Re-exports
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
import qualified Data.Aeson as A
import Data.Coerce (coerce)
import qualified Data.HashMap.Lazy as H
import qualified Data.Map as M
import qualified Data.Scientific as Sci
import Data.Vector (Vector)
import qualified Data.Vector as V
import Foreign (nullPtr)
import qualified Foreign.C.String
import Hercules.CNix.Encapsulation (moveToForeignPtrWrapper)
import Hercules.CNix.Expr.Context
import Hercules.CNix.Expr.Raw
import Hercules.CNix.Expr.Typed
import Hercules.CNix.Store
import Hercules.CNix.Store.Context
import qualified Hercules.CNix as CNix
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Protolude hiding (evalState)
import System.Directory (makeAbsolute)
import Data.Aeson.KeyMap (toMapText)
import qualified Data.ByteString.Unsafe as BS

C.context (Hercules.CNix.Store.Context.context <> Hercules.CNix.Expr.Context.evalContext)

C.verbatim "#define GC_THREADS 1"

C.include "<stdio.h>"

C.include "<cstring>"

C.include "<math.h>"

C.include "<nix/util/config-global.hh>"
C.include "<nix/store/store-api.hh>"
C.include "<nix/store/globals.hh>"
C.include "<nix/expr/eval.hh>"
C.include "<nix/expr/eval-inline.hh>"
C.include "<nix/expr/get-drvs.hh>"
C.include "<nix/flake/flake.hh>"
C.include "<nix/flake/flakeref.hh>"
C.include "<nix/flake/settings.hh>"
C.include "<nix/fetchers/fetch-settings.hh>"
C.include "<nix/main/shared.hh>"

C.include "hercules-ci-cnix/expr.hxx"

C.include "hercules-ci-cnix/string.hxx"

C.include "<gc/gc.h>"

C.include "<gc/gc_cpp.h>"

C.include "<gc/gc_allocator.h>"

C.using "namespace nix"

C.using "namespace hercules_ci_cnix"

C.verbatim "\nGC_API void GC_CALL GC_throw_bad_alloc() { throw std::bad_alloc(); }\n"

C.verbatim "nix::flake::Settings flakeSettings;"
C.verbatim "nix::fetchers::Settings fetchSettings;"
C.verbatim "static bool readOnlyMode = false;"
C.verbatim "nix::EvalSettings evalSettings(readOnlyMode);"

init :: IO ()
init = do
  [C.throwBlock| void {
    { auto _ = GlobalConfig::Register(&flakeSettings); }
    { auto _ = GlobalConfig::Register(&fetchSettings); }
    { auto _ = GlobalConfig::Register(&evalSettings); }
  }|]
  CNix.init
  [C.throwBlock| void {
    nix::initGC();
    globalConfig.set("extra-experimental-features", "flakes");
    flakeSettings.configureEvalSettings(evalSettings);
  }|]

-- | Initialize the current (main) thread for stack overflow detection.
initThread :: IO ()
initThread =
  void
    [C.throwBlock| void {
      nix::detectStackOverflow();
    }|]

-- | Configure the garbage collector to support threads.
--
-- This is not needed when all evaluation happens on the main thread.
allowThreads :: IO ()
allowThreads =
  void
    [C.block| void {
      GC_allow_register_threads();
    }|]

-- | Run in a thread from which GC may be triggered safely.
--
-- This also installs the stack overflow handler.
--
-- NOTE: Before using this, you must call 'allowThreads' once.
runGcRegisteredThread :: IO a -> IO a
runGcRegisteredThread io =
  runInBoundThread do
    bracket
      start
      (const end)
      (const io)
  where
    start =
      do
        initThread
        [C.block| void {
          struct GC_stack_base sb;
          int r = GC_get_stack_base(&sb);
          assert(r == GC_SUCCESS);
          GC_register_my_thread(&sb);
        }|]
        pass
    end =
      do
        [C.block| void {
          GC_unregister_my_thread();
        }|]
        pass


{- | Configure the stack overflow handler to sleep before returning, allowing
  other threads to continue for a bit.

  No-op before Nix 2.12
-}
setExtraStackOverflowHandlerToSleep :: IO ()
setExtraStackOverflowHandlerToSleep =
  void
    [C.throwBlock| void {
      nix::stackOverflowHandler = [](siginfo_t *info, void *ctx) {
        Error error("stack overflow");
        logError(error.info());

        // This is risky for a signal handler.
        // Note that the original thread is now in a permanently blocked state
        // so we can easily create a deadlock.
        // Allow the rest of the process to continue for a bit.
        sleep(1);
        _exit(1);
        };
    }|]

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

-- | (private) Make an EvalState and leak it.
newEvalState :: MonadIO m => Store -> m (Ptr EvalState)
newEvalState (Store store) = liftIO
  [C.throwBlock| EvalState* {
    nix::LookupPath emptyLookupPath;
    return new EvalState(emptyLookupPath, *$(refStore* store), fetchSettings, evalSettings);
  } |]

-- | (private) Don't leak it.
deleteEvalState :: MonadIO m => (Ptr EvalState) -> m ()
deleteEvalState st = liftIO [C.throwBlock| void { delete $(EvalState* st); } |]

withEvalState ::
  Store ->
  (Ptr EvalState -> IO a) ->
  IO a
withEvalState store = bracket (newEvalState store) deleteEvalState

withEvalStateConduit ::
  MonadResource m =>
  Store ->
  (Ptr EvalState -> ConduitT i o m r) ->
  ConduitT i o m r
withEvalStateConduit store = bracketP (newEvalState store) deleteEvalState

-- | Insert an allowed path. Only has an effect when in restricted or pure mode.
addAllowedPath :: Ptr EvalState -> ByteString -> IO ()
addAllowedPath evalState path =
  [C.throwBlock| void {
    std::string path = std::string($bs-ptr:path, $bs-len:path);
    EvalState &evalState = *$(EvalState *evalState);
    evalState.allowPath(path);
  }|]

addInternalAllowedPaths :: Ptr EvalState -> IO ()
addInternalAllowedPaths _evalState = do
  pass

evalFile :: Ptr EvalState -> FilePath -> IO RawValue
evalFile evalState filename = do
  filename' <- Foreign.C.String.newCString filename
  mkRawValue
    =<< [C.throwBlock| Value* {
      EvalState & state = *$(EvalState *evalState);
      Value value;
      auto cstr = $(const char *filename');
      SourcePath path {state.rootPath(CanonPath(cstr))};
      state.evalFile(path, value);
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

autoCallFunction :: Ptr EvalState -> RawValue -> Value NixAttrs -> IO RawValue
autoCallFunction evalState (RawValue fun) (Value (RawValue autoArgs)) =
  mkRawValue
    =<< [C.throwBlock| Value* {
          Value result;
          $(EvalState *evalState)->autoCallFunction(
                  const_cast<Bindings &>(*$(Value *autoArgs)->attrs()),
                  *$(Value *fun),
                  result);
          return new (NoGC) Value (result);
        }|]

isDerivation :: Ptr EvalState -> RawValue -> IO Bool
isDerivation evalState (RawValue v) =
  (0 /=)
    <$> [C.throwBlock| int {
          if ($(Value *v) == NULL) { throw std::invalid_argument("forceValue value must be non-null"); }
          $(EvalState *evalState)->forceValue(*$(Value *v), nix::noPos);
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
          auto attrs = v->attrs();
          // Bindings::const_iterator iter; // 2.24
          auto iter = attrs->find(evalState.sRecurseForDerivations);
          if (iter == attrs->end()) {
            return 0;
          } else {
            // Previously this bool was unpacked manually and included a special
            // case to return true when it is not a bool. That logic was added
            // because an empty attrset was found here, observed in
            // nixpkgs master 67e2de195a4aa0a50ffb1e1ba0b4fb531dca67dc
            return evalState.forceBool(*iter->value, iter->pos, "while evaluating whether to traverse into an attribute set to find more derivations");
          }
        } |]

getAttr :: Ptr EvalState -> Value NixAttrs -> ByteString -> IO (Maybe RawValue)
getAttr evalState (Value (RawValue v)) k =
  mkNullableRawValue
    =<< [C.throwBlock| Value *{
      Value &v = *$(Value *v);
      EvalState &evalState = *$(EvalState *evalState);
      Symbol k = evalState.symbols.create($bs-cstr:k);
      auto attrs = v.attrs();
      // Bindings::const_iterator iter; // 2.24
      auto iter = attrs->find(k);
      if (iter == attrs->end()) {
        return nullptr;
      } else {
        return iter->value;
      }
    }|]

-- | Converts 'nullPtr' to 'Nothing'; actual values to @Just (a :: 'RawValue')@
mkNullableRawValue :: Ptr Value' -> IO (Maybe RawValue)
mkNullableRawValue p | p == nullPtr = pure Nothing
mkNullableRawValue p = Just <$> mkRawValue p

getAttrs :: Ptr EvalState -> Value NixAttrs -> IO (Map ByteString RawValue)
getAttrs evalState (Value (RawValue v)) = do
  begin <- [C.exp| const Attr *{ $(Value *v)->attrs()->begin() }|]
  end <- [C.exp| const Attr *{ $(Value *v)->attrs()->end() }|]
  let gather :: Map ByteString RawValue -> Ptr Attr' -> IO (Map ByteString RawValue)
      gather acc i | i == end = pure acc
      gather acc i = do
        name <- BS.unsafePackMallocCString =<< [C.block| const char *{
          EvalState &evalState = *$(EvalState *evalState);
          SymbolStr str = evalState.symbols[$(Attr *i)->name];
          return stringdup(static_cast<std::string>(str));
        }|]
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
        throw EvalError(state, "Not a valid derivation");

      StorePath storePath = drvInfo->requireDrvPath();

      // write it (?)
      auto drv = state.store->derivationFromPath(storePath);

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

-- | Parse a string and eval it.
valueFromExpressionString ::
  Ptr EvalState ->
  -- | The string to parse
  ByteString ->
  -- | Base path for path exprs
  ByteString ->
  IO RawValue
valueFromExpressionString evalState s basePath = do
  mkRawValue
    =<< [C.throwBlock| Value *{
      EvalState &evalState = *$(EvalState *evalState);
      std::string basePathStr = std::string($bs-ptr:basePath, $bs-len:basePath);
      SourcePath basePath {evalState.rootPath(CanonPath(basePathStr))};
      Expr *expr = evalState.parseExprFromString(std::string($bs-ptr:s, $bs-len:s), basePath);
      Value *r = new (NoGC) Value();
      evalState.eval(expr, *r);
      return r;
  }|]

-- | 'apply' but strict.
callFunction :: Ptr EvalState -> RawValue -> RawValue -> IO RawValue
callFunction evalState (RawValue f) (RawValue a) = do
  mkRawValue
    =<< [C.throwBlock| Value *{
      EvalState &evalState = *$(EvalState *evalState);
      Value *r = new (NoGC) Value();
      evalState.callFunction(*$(Value *f), *$(Value *a), *r, noPos);
      return r;
  }|]

apply :: RawValue -> RawValue -> IO RawValue
apply (RawValue f) (RawValue a) = do
  mkRawValue
    =<< [C.throwBlock| Value *{
      Value *r = new (NoGC) Value();
      r->mkApp($(Value *f), $(Value *a));
      return r;
    }|]

mkPath :: Ptr EvalState -> ByteString -> IO (Value NixPath)
mkPath evalState path =
  Value
    <$> ( mkRawValue
            =<< [C.throwBlock| Value *{
      Value *r = new (NoGC) Value();
      std::string s($bs-ptr:path, $bs-len:path);
      EvalState & state = *$(EvalState *evalState);
      r->mkPath(state.rootPath(CanonPath(s)));
      return r;
  }|]
        )

getFlakeFromFlakeRef :: Ptr EvalState -> ByteString -> IO RawValue
getFlakeFromFlakeRef evalState flakeRef = do
  [C.throwBlock| Value *{
    EvalState &evalState = *$(EvalState *evalState);
    Value *r = new (NoGC) Value();
    std::string flakeRefStr($bs-ptr:flakeRef, $bs-len:flakeRef);
    auto flakeRef = nix::parseFlakeRef(
      fetchSettings,
      flakeRefStr,
      {},
      true);
    nix::flake::callFlake(evalState,
      nix::flake::lockFlake(
        flakeSettings,
        evalState,
        flakeRef,
        nix::flake::LockFlags {
          .updateLockFile = false,
          .useRegistries = false,
          .allowUnlocked = false,
        }),
      *r);
    return r;
  }|]
    >>= mkRawValue

getLocalFlake :: Ptr EvalState -> Text -> IO RawValue
getLocalFlake evalState path = do
  absPath <- encodeUtf8 . toS <$> makeAbsolute (toS path)
  mkRawValue
    =<< [C.throwBlock| Value *{
    EvalState &evalState = *$(EvalState *evalState);
    Value *r = new (NoGC) Value();
    std::string path($bs-ptr:absPath, $bs-len:absPath);
    auto flakeRef = nix::parseFlakeRef(
      fetchSettings,
      path,
      {},
      true);
    nix::flake::callFlake(evalState,
      nix::flake::lockFlake(
        flakeSettings,
        evalState,
        flakeRef,
        nix::flake::LockFlags {
          .updateLockFile = false,
          .useRegistries = false,
          .allowUnlocked = false,
        }),
      *r);
    return r;
  }|]

getFlakeFromGit :: Ptr EvalState -> Text -> Text -> Text -> IO RawValue
getFlakeFromGit evalState url ref rev =
  let
    urlb = encodeUtf8 url
    refb = encodeUtf8 ref
    revb = encodeUtf8 rev
  in [C.throwBlock| Value *{
    EvalState &evalState = *$(EvalState *evalState);
    Value *r = new (NoGC) Value();
    std::string url($bs-ptr:urlb, $bs-len:urlb);
    std::string ref($bs-ptr:refb, $bs-len:refb);
    std::string rev($bs-ptr:revb, $bs-len:revb);

    fetchers::Attrs attrs;
    attrs.emplace("type", "git");
    attrs.emplace("url", url);
    attrs.emplace("ref", ref);
    attrs.emplace("rev", rev);

    auto flakeRef = nix::FlakeRef::fromAttrs(
      fetchSettings,
      attrs);
    nix::flake::callFlake(evalState,
      nix::flake::lockFlake(
        flakeSettings,
        evalState,
        flakeRef,
        nix::flake::LockFlags {
          .updateLockFile = false,
          .useRegistries = false,
          .allowUnlocked = false,
        }),
      *r);
    return r;
  }|]
    >>= mkRawValue

getFlakeFromArchiveUrl :: Ptr EvalState -> Text -> IO RawValue
getFlakeFromArchiveUrl evalState url = do
  srcArgs <-
    toRawValue evalState $
      ("url" :: ByteString) =: url
  fn <- valueFromExpressionString evalState "builtins.fetchTarball" "/"
  pValue <- apply fn srcArgs
  p <- assertType evalState pValue
  p' <- getStringIgnoreContext p
  getFlakeFromFlakeRef evalState p'

traverseWithKey_ :: Applicative f => (k -> a -> f ()) -> Map k a -> f ()
traverseWithKey_ f = M.foldrWithKey (\k a more -> f k a *> more) (pure ())

class ToRawValue a where
  toRawValue :: Ptr EvalState -> a -> IO RawValue
  default toRawValue :: ToValue a => Ptr EvalState -> a -> IO RawValue
  toRawValue evalState a = rtValue <$> toValue evalState a

class ToRawValue a => ToValue a where
  type NixTypeFor a :: Type
  toValue :: Ptr EvalState -> a -> IO (Value (NixTypeFor a))

-- | Marshall values from Nix into Haskell. Instances must satisfy the
-- requirements that:
--
--  - Only a single Nix value type is acceptable for the Haskell type.
--  - Marshalling does not fail, as the Nix runtime type has already been checked.
class FromValue n a | a -> n where
  fromValue :: Value n -> IO a

instance FromValue Bool Bool where
  fromValue = getBool

instance FromValue NixList [RawValue] where
  fromValue = getList

instance FromValue NixInt Int64 where
  fromValue = getInt

-- | Identity
instance ToRawValue RawValue where
  toRawValue _ = pure

-- | Upcast
instance forall (a :: Type). ToRawValue (Value a)

-- | Identity
instance forall (a :: Type). ToValue (Value a) where
  type NixTypeFor (Value a) = a
  toValue _ = pure

instance ToRawValue C.CBool

instance ToValue C.CBool where
  type NixTypeFor C.CBool = Bool
  toValue _ b =
    coerce
      <$> [C.block| Value *{
      Value *r = new (NoGC) Value();
      r->mkBool($(bool b));
      return r;
    }|]

instance ToRawValue Bool

instance ToValue Bool where
  type NixTypeFor Bool = Bool
  toValue es False = toValue es (0 :: C.CBool)
  toValue es True = toValue es (1 :: C.CBool)

-- | The native Nix integer type
instance ToRawValue Int64

-- | The native Nix integer type
instance ToValue Int64 where
  type NixTypeFor Int64 = NixInt
  toValue _ i =
    coerce
      <$> [C.block| Value *{
    Value *r = new (NoGC) Value();
    r->mkInt($(int64_t i));
    return r;
  }|]

instance ToRawValue Int

instance ToValue Int where
  type NixTypeFor Int = NixInt
  toValue es i = toValue es (fromIntegral i :: Int64)

instance ToRawValue C.CDouble

instance ToValue C.CDouble where
  type NixTypeFor C.CDouble = NixFloat
  toValue _ f =
    coerce
      <$> [C.block| Value *{
        Value *r = new (NoGC) Value();
        r->mkFloat($(double f));
        return r;
      }|]

instance ToRawValue Double

instance ToValue Double where
  type NixTypeFor Double = NixFloat
  toValue es f = toValue es (fromRational (toRational f) :: C.CDouble)

-- | Nix String
instance ToValue ByteString where
  type NixTypeFor ByteString = NixString
  toValue _ s =
    -- TODO simplify when r->mkString(string_view) is safe in all supported Nix versions
    coerce
      <$> [C.block| Value *{
    Value *r = new (NoGC) Value();
    std::string_view s($bs-ptr:s, $bs-len:s);
    // If empty, the pointer may be invalid; don't use it.
    if (s.size() == 0) {
      r->mkString("");
    }
    else {
      r->mkString(GC_STRNDUP(s.data(), s.size()));
    }
    return r;
  }|]

-- | Nix String
instance ToRawValue ByteString

-- | UTF-8
instance ToRawValue Text

-- | UTF-8
instance ToValue Text where
  type NixTypeFor Text = NixString
  toValue es s = toValue es (encodeUtf8 s)

instance ToRawValue a => ToRawValue (Map ByteString a)

withBindingsBuilder :: Integral n => Ptr EvalState -> n -> (Ptr BindingsBuilder' -> IO ()) -> IO (Value NixAttrs)
withBindingsBuilder evalState n f = do
  withBindingsBuilder' evalState n \bb -> do
    f bb
    v <- [C.block| Value* {
      auto v = new (NoGC) Value();
      v->mkAttrs(*$(BindingsBuilder *bb));
      return v;
    }|]
    Value <$> mkRawValue v

withBindingsBuilder' :: Integral n => Ptr EvalState -> n -> (Ptr BindingsBuilder' -> IO a) -> IO a
withBindingsBuilder' evalState n =
  let l :: C.CInt
      l = fromIntegral n
  in
    bracket
      [C.block| BindingsBuilder* {
        auto &evalState = *$(EvalState *evalState);
        return new BindingsBuilder(evalState, evalState.allocBindings($(int l)));
      }|]
      \bb -> [C.block| void { delete $(BindingsBuilder *bb); }|]

instance ToRawValue a => ToValue (Map ByteString a) where
  type NixTypeFor (Map ByteString a) = NixAttrs

  toValue evalState attrs = withBindingsBuilder evalState (length attrs) \bb -> do
    attrs & traverseWithKey_ \k a -> do
      RawValue aRaw <- toRawValue evalState a
      [C.block| void {
          EvalState &evalState = *$(EvalState *evalState);
          std::string k($bs-ptr:k, $bs-len:k);
          Value &a = *$(Value *aRaw);
          $(BindingsBuilder *bb)->alloc(evalState.symbols.create(k)) = a;
        }|]

instance ToRawValue a => ToRawValue (Map Text a)

instance ToRawValue a => ToValue (Map Text a) where
  type NixTypeFor (Map Text a) = NixAttrs
  toValue evalState attrs = toValue evalState (M.mapKeys encodeUtf8 attrs)

mkNull :: IO RawValue
mkNull =
  coerce
    <$> [C.block| Value* {
          Value *v = new (NoGC) Value();
          v->mkNull();
          return v;
        }|]

instance ToRawValue A.Value where
  toRawValue es (A.Bool b) = toRawValue es b
  toRawValue es (A.String s) = toRawValue es s
  toRawValue es (A.Object fs) = toRawValue es $ toMapText fs
  toRawValue _es A.Null = mkNull
  toRawValue es (A.Number n) | Just i <- Sci.toBoundedInteger n = toRawValue es (i :: Int64)
  toRawValue es (A.Number f) = toRawValue es (Sci.toRealFloat f :: Double)
  toRawValue es (A.Array a) = toRawValue es a

-- | For deriving-via of 'ToRawValue' using 'ToJSON'.
newtype ViaJSON a = ViaJSON {fromViaJSON :: a}
  deriving newtype (Eq, Ord, Read, Show)

instance A.ToJSON a => ToRawValue (ViaJSON a) where
  toRawValue es (ViaJSON a) = toRawValue es (A.toJSON a)

hmTraverseWithKey_ :: Applicative f => (k -> a -> f ()) -> H.HashMap k a -> f ()
hmTraverseWithKey_ f = H.foldrWithKey (\k a more -> f k a *> more) (pure ())

instance ToRawValue a => ToRawValue (H.HashMap Text a)

instance ToRawValue a => ToValue (H.HashMap Text a) where
  type NixTypeFor (H.HashMap Text a) = NixAttrs

  toValue evalState attrs = withBindingsBuilder evalState (length attrs) \bb -> do
    attrs & hmTraverseWithKey_ \k' a -> do
      RawValue aRaw <- toRawValue evalState a
      let k = encodeUtf8 k'
      [C.block| void {
          EvalState &evalState = *$(EvalState *evalState);
          std::string k($bs-ptr:k, $bs-len:k);
          Value &a = *$(Value *aRaw);
          $(BindingsBuilder *bb)->alloc(evalState.symbols.create(k)) = a;
        }|]

instance ToRawValue a => ToRawValue (Vector a)

instance ToRawValue a => ToValue (Vector a) where
  type NixTypeFor (Vector a) = NixList
  toValue evalState vec =
    coerce <$> do
      let l :: C.CInt
          l = fromIntegral (length vec)
      b <-
        [C.block| ListBuilder* {
          EvalState &evalState = *$(EvalState *evalState);
          return new (NoGC) ListBuilder(evalState, $(int l));
        }|]
      vec & V.imapM_ \i a -> do
        RawValue aRaw <- toRawValue evalState a
        let ix = fromIntegral i
        [C.block| void {
          ListBuilder &b = *$(ListBuilder *b);
          b[$(int ix)] = $(Value *aRaw);
        }|]
      v <-
        [C.block| Value* {
          Value * v = new (NoGC) Value();
          v->mkList(*$(ListBuilder *b));
          return v;
        }|]
      Value <$> mkRawValue v

instance ToRawValue a => ToRawValue [a]

instance ToRawValue a => ToValue [a] where
  type NixTypeFor [a] = NixList
  toValue es l = toValue es (V.fromList l)
