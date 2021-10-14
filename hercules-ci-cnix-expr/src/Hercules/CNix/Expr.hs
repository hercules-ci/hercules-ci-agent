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
import qualified Data.Aeson as A
import qualified Data.ByteString.Unsafe as BS
import Data.Coerce (coerce)
import qualified Data.HashMap.Lazy as H
import qualified Data.Map as M
import qualified Data.Scientific as Sci
import Data.Vector (Vector)
import qualified Data.Vector as V
import Foreign (allocaArray, nullPtr, peekArray, toBool)
import qualified Foreign.C.String
import Hercules.CNix.Encapsulation (moveToForeignPtrWrapper)
import Hercules.CNix.Expr.Context
import Hercules.CNix.Expr.Raw
import Hercules.CNix.Expr.Typed
import Hercules.CNix.Store
import Hercules.CNix.Store.Context
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import Paths_hercules_ci_cnix_expr (getDataFileName)
import Protolude hiding (evalState)
import System.Directory (makeAbsolute)

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

#ifdef NIX_2_4

C.include "<nix/flake/flake.hh>"

C.include "<nix/flake/flakeref.hh>"

#endif

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
#ifdef NIX_2_4
      Strings features(nix::settings.experimentalFeatures.get());
      features.push_back("flakes");
      nix::settings.experimentalFeatures.assign(features);
#endif
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

-- | Insert an allowed path. Only has an effect when in restricted or pure mode.
addAllowedPath :: Ptr EvalState -> ByteString -> IO ()
addAllowedPath evalState path =
  [C.throwBlock| void {
    std::string path = std::string($bs-ptr:path, $bs-len:path);
    EvalState &evalState = *$(EvalState *evalState);
    if (evalState.allowedPaths) {
      evalState.allowedPaths->insert(path);
    }
  }|]

addInternalAllowedPaths :: Ptr EvalState -> IO ()
addInternalAllowedPaths evalState = do
  addAllowedPath evalState . encodeUtf8 . toS =<< getDataFileName "vendor/flake-compat"

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
      Expr *expr = evalState.parseExprFromString(std::string($bs-ptr:s, $bs-len:s), std::string($bs-ptr:basePath, $bs-len:basePath));
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

#ifdef NIX_2_4
    =<< [C.throwBlock| Value *{
      Value *r = new (NoGC) Value();
      r->mkApp($(Value *f), $(Value *a));
      return r;
    }|]
#else
    =<< [C.throwBlock| Value *{
      Value *r = new (NoGC) Value();
      r->type = tApp;
      r->app.left = $(Value *f);
      r->app.right = $(Value *a);
      return r;
    }|]
#endif

mkPath :: ByteString -> IO (Value NixPath)
mkPath path =
  Value
    <$> ( mkRawValue
            =<< [C.throwBlock| Value *{
      Value *r = new (NoGC) Value();
      std::string s($bs-ptr:path, $bs-len:path);
      mkPath(*r, s.c_str());
      return r;
  }|]
        )

#ifdef NIX_2_4

getFlakeFromFlakeRef :: Ptr EvalState -> ByteString -> IO RawValue
getFlakeFromFlakeRef evalState flakeRef = do
  [C.throwBlock| Value *{
    EvalState &evalState = *$(EvalState *evalState);
    Value *r = new (NoGC) Value();
    std::string flakeRefStr($bs-ptr:flakeRef, $bs-len:flakeRef);
    auto flakeRef = nix::parseFlakeRef(flakeRefStr, {}, true);
    nix::flake::callFlake(evalState,
      nix::flake::lockFlake(evalState, flakeRef,
        nix::flake::LockFlags {
          .updateLockFile = false,
          .useRegistries = false,
          .allowMutable = false,
        }),
      *r);
    return r;
  }|] >>= mkRawValue

#else

getFlakeCompat :: Ptr EvalState -> IO (Value NixFunction)
getFlakeCompat evalState = do
  getDataFileName "vendor/flake-compat/default.nix"
    >>= makeAbsolute
    >>= evalFile evalState
    >>= assertType evalState

#endif

getLocalFlake :: Ptr EvalState -> Text -> IO RawValue
#ifdef NIX_2_4
getLocalFlake evalState path = do
  absPath <- encodeUtf8 . toS <$> makeAbsolute (toS path)
  mkRawValue =<< [C.throwBlock| Value *{
    EvalState &evalState = *$(EvalState *evalState);
    Value *r = new (NoGC) Value();
    std::string path($bs-ptr:absPath, $bs-len:absPath);
    auto flakeRef = nix::parseFlakeRef(path, {}, true);
    nix::flake::callFlake(evalState,
      nix::flake::lockFlake(evalState, flakeRef,
        nix::flake::LockFlags {
          .updateLockFile = false,
          .useRegistries = false,
          .allowMutable = false,
        }),
      *r);
    return r;
  }|]
#else
getLocalFlake evalState path = do
  flakeCompat <- getFlakeCompat evalState
  (Value (RawValue flakeSource)) <- mkPath . encodeUtf8 . toS =<< makeAbsolute (toS path)
  flakeCompatArgs <- mkRawValue =<< [C.throwBlock| Value * {
    EvalState &evalState = *$(EvalState *evalState);
    Value *r = new (NoGC) Value();
    evalState.mkAttrs(*r, 1);
    Symbol sSrc = evalState.symbols.create("src");
    *evalState.allocAttr(*r, sSrc) = *$(Value *flakeSource);
    r->attrs->sort();
    return r;
  }|]
  flakeCompatResult <- apply (rtValue flakeCompat) flakeCompatArgs
  flakeCompatAttrs <- match evalState flakeCompatResult >>= \case
    Right (IsAttrs a) -> pure a
    Left e -> throwIO e
    _ -> panic "flake-compat must return attrs"
  getAttr evalState flakeCompatAttrs "defaultNix"
    <&> fromMaybe (panic "flake-compat must have defaultNix attr")
#endif

getFlakeFromGit :: Ptr EvalState -> Text -> Text -> Text -> IO RawValue
#ifdef NIX_2_4
getFlakeFromGit evalState url ref rev = do
  -- TODO: use a URL library
  getFlakeFromFlakeRef evalState (encodeUtf8 $ url <> "?ref=" <> ref <> "&rev=" <> rev)
#else
getFlakeFromGit evalState url ref rev = do
  srcArgs <-
    toValue evalState $
      ("url" :: ByteString) =: url
        <> "ref" =: ref
        <> "rev" =: rev
  flakeCompat <- getFlakeCompat evalState
  args <- toRawValue evalState =<< sequenceA (
        ("srcArgs" :: ByteString) =: toRawValue evalState srcArgs
      <> "flake-compat" =: toRawValue evalState flakeCompat
    )
  fn <- valueFromExpressionString evalState "{srcArgs, flake-compat}: (flake-compat { src = builtins.fetchGit srcArgs; }).defaultNix" "/"
  apply fn args
#endif

getFlakeFromArchiveUrl :: Ptr EvalState -> Text -> IO RawValue
#ifdef NIX_2_4
getFlakeFromArchiveUrl evalState url = do
  srcArgs <-
    toRawValue evalState $
      ("url" :: ByteString) =: url
  fn <- valueFromExpressionString evalState "builtins.fetchTarball" "/"
  pValue <- apply fn srcArgs
  p <- assertType evalState pValue
  p' <- getStringIgnoreContext p
  getFlakeFromFlakeRef evalState p'
#else
getFlakeFromArchiveUrl evalState url = do
  srcArgs <-
    toValue evalState $
      ("url" :: ByteString) =: url
  flakeCompat <- getFlakeCompat evalState
  args <- toRawValue evalState =<< sequenceA (
        ("srcArgs" :: ByteString) =: toRawValue evalState srcArgs
      <> "flake-compat" =: toRawValue evalState flakeCompat
    )
  fn <- valueFromExpressionString evalState "{srcArgs, flake-compat}: (flake-compat { src = builtins.fetchTarball srcArgs; }).defaultNix" "/"
  apply fn args
#endif

traverseWithKey_ :: Applicative f => (k -> a -> f ()) -> Map k a -> f ()
traverseWithKey_ f = M.foldrWithKey (\k a more -> f k a *> more) (pure ())

class ToRawValue a where
  toRawValue :: Ptr EvalState -> a -> IO RawValue
  default toRawValue :: ToValue a => Ptr EvalState -> a -> IO RawValue
  toRawValue evalState a = rtValue <$> toValue evalState a

class ToRawValue a => ToValue a where
  type NixTypeFor a :: *
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

-- | Identity
instance ToRawValue RawValue where
  toRawValue _ = pure

-- | Upcast
instance ToRawValue (Value a)

-- | Identity
instance ToValue (Value a) where
  type NixTypeFor (Value a) = a
  toValue _ = pure

instance ToRawValue C.CBool

instance ToValue C.CBool where
  type NixTypeFor C.CBool = Bool
  toValue _ b =
    coerce
      <$> [C.block| Value *{
      Value *r = new (NoGC) Value();
      mkBool(*r, $(bool b));
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
    mkInt(*r, $(int64_t i));
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
        mkFloat(*r, $(double f));
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
    coerce
      <$> [C.block| Value *{
    Value *r = new (NoGC) Value();
    std::string s($bs-ptr:s, $bs-len:s);
    mkString(*r, s, {});
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

instance ToRawValue a => ToValue (Map ByteString a) where
  type NixTypeFor (Map ByteString a) = NixAttrs
  toValue evalState attrs = do
    let l :: C.CInt
        l = fromIntegral (length attrs)
    v <-
      [C.block| Value* {
          EvalState &evalState = *$(EvalState *evalState);
          Value *v = new (NoGC) Value();
          evalState.mkAttrs(*v, $(int l));
          return v;
        }|]
    attrs & traverseWithKey_ \k a -> do
      RawValue aRaw <- toRawValue evalState a
      [C.block| void {
          EvalState &evalState = *$(EvalState *evalState);
          std::string k($bs-ptr:k, $bs-len:k);
          Value &a = *$(Value *aRaw);
          *evalState.allocAttr(*$(Value *v), evalState.symbols.create(k)) = a;
        }|]
    [C.block| void {
        $(Value *v)->attrs->sort();
      }|]
    Value <$> mkRawValue v

instance ToRawValue a => ToRawValue (Map Text a)

instance ToRawValue a => ToValue (Map Text a) where
  type NixTypeFor (Map Text a) = NixAttrs
  toValue evalState attrs = toValue evalState (M.mapKeys encodeUtf8 attrs)

mkNull :: IO RawValue
mkNull =
  coerce
    <$> [C.block| Value* {
          Value *v = new (NoGC) Value();
          mkNull(*v);
          return v;
        }|]

instance ToRawValue A.Value where
  toRawValue es (A.Bool b) = toRawValue es b
  toRawValue es (A.String s) = toRawValue es s
  toRawValue es (A.Object fs) = toRawValue es fs
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
  toValue evalState attrs = do
    let l :: C.CInt
        l = fromIntegral (length attrs)
    v <-
      [C.block| Value* {
          EvalState &evalState = *$(EvalState *evalState);
          Value *v = new (NoGC) Value();
          evalState.mkAttrs(*v, $(int l));
          return v;
        }|]
    attrs & hmTraverseWithKey_ \k' a -> do
      RawValue aRaw <- toRawValue evalState a
      let k = encodeUtf8 k'
      [C.block| void {
          EvalState &evalState = *$(EvalState *evalState);
          std::string k($bs-ptr:k, $bs-len:k);
          Value &a = *$(Value *aRaw);
          *evalState.allocAttr(*$(Value *v), evalState.symbols.create(k)) = a;
        }|]
    [C.block| void {
        $(Value *v)->attrs->sort();
      }|]
    Value <$> mkRawValue v

instance ToRawValue a => ToRawValue (Vector a)

instance ToRawValue a => ToValue (Vector a) where
  type NixTypeFor (Vector a) = NixList
  toValue evalState vec =
    coerce <$> do
      let l :: C.CInt
          l = fromIntegral (length vec)
      v <-
        [C.block| Value* {
          EvalState &evalState = *$(EvalState *evalState);
          Value *v = new (NoGC) Value();
          evalState.mkList(*v, $(int l));
          return v;
        }|]
      vec & V.imapM_ \i a -> do
        RawValue aRaw <- toRawValue evalState a
        let ix = fromIntegral i
        [C.block| void {
          Value &v = *$(Value *v);
          v.listElems()[$(int ix)] = $(Value *aRaw);
        }|]
      Value <$> mkRawValue v

instance ToRawValue a => ToRawValue [a]

instance ToRawValue a => ToValue [a] where
  type NixTypeFor [a] = NixList
  toValue es l = toValue es (V.fromList l)

data FunctionParams = FunctionParams
  { functionArgName :: Maybe ByteString,
    functionParamsMatches :: Maybe FunctionMatches
  }
  deriving (Eq, Show)

data FunctionMatches = FunctionMatches
  { functionMatches :: Map ByteString Bool,
    functionMatchesEllipsis :: Bool
  }
  deriving (Eq, Show)

functionParams :: Ptr EvalState -> Value NixFunction -> IO FunctionParams
functionParams _evalState (Value (RawValue v)) = do
#ifdef NIX_2_4
  isLambda <-
    toBool <$> [C.exp| bool { $(Value *v)->isLambda() }|]
#else
  isLambda <-
    toBool <$> [C.exp| bool { $(Value *v)->type == tLambda }|]
#endif
  if not isLambda
    then pure $ FunctionParams Nothing Nothing
    else do
      isMatchAttrs <-
        toBool <$> [C.exp| bool { $(Value *v)->lambda.fun->matchAttrs }|]
      argName' <-
        traverseNonNull BS.unsafePackMallocCString
          =<< [C.throwBlock| const char* {
            Value &v = *$(Value *v);
            if (!v.lambda.fun->arg.set())
              return nullptr;
            const std::string &s = v.lambda.fun->arg;
            return strdup(s.c_str());
          }|]
      let argName = if argName' == Just "" then Nothing else argName'
      if not isMatchAttrs
        then do
          pure
            FunctionParams
              { functionArgName = argName,
                functionParamsMatches = Nothing
              }
        else do
          size <-
            [C.block| size_t {
              Value &v = *$(Value *v);
              return v.lambda.fun->formals->formals.size();
            }|]
          formals <-
            if size == 0
              then pure []
              else allocaArray (fromIntegral size) \namesArr -> allocaArray (fromIntegral size) \defsArr -> do
                [C.throwBlock| void {
              Value &v = *$(Value *v);
              char **names = $(char **namesArr);
              bool *defs = $(bool *defsArr);
              size_t i = 0;
              size_t sz = $(size_t size);
              for (auto &formal : v.lambda.fun->formals->formals) {
                assert(i < sz);
                assert(formal.name.set());
                const std::string &s = formal.name;
                names[i] = strdup(s.c_str());
                defs[i] = formal.def != nullptr;
                i++;
              }
              assert(i == sz);
            }|]
                cstrings <- peekArray (fromIntegral size) namesArr
                defs <- peekArray (fromIntegral size) defsArr
                for (zip cstrings defs) \(cstring, def) -> do
                  (,toBool def) <$> BS.unsafePackMallocCString cstring
          ellipsis <- toBool <$> [C.exp| bool { $(Value *v)->lambda.fun->formals->ellipsis }|]
          pure $
            FunctionParams argName $
              Just
                FunctionMatches
                  { functionMatches = M.fromList formals,
                    functionMatchesEllipsis = ellipsis
                  }
