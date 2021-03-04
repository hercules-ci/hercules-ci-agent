{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.CNix.Store
  ( module Hercules.CNix.Store,
    module Hercules.CNix.Store.Context,
  )
where

import Control.Exception
import Control.Monad.IO.Unlift
import Data.ByteString.Unsafe (unsafePackMallocCString)
import qualified Data.ByteString.Unsafe as BS
import Data.Coerce (coerce)
import qualified Data.Map as M
import Foreign.ForeignPtr
import Hercules.CNix.Store.Context
  ( Derivation,
    DerivationInputsIterator,
    DerivationOutputsIterator,
    NixStore,
    PathSetIterator,
    Ref,
    SecretKey,
    StringPairs,
    Strings,
    ValidPathInfo,
    context,
    unsafeMallocBS,
  )
import qualified Hercules.CNix.Store.Context as C hiding (context)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import Protolude
import System.IO.Unsafe (unsafePerformIO)
import Prelude ()

C.context context

C.include "<cstring>"

C.include "<nix/config.h>"

C.include "<nix/shared.hh>"

C.include "<nix/store-api.hh>"

C.include "<nix/get-drvs.hh>"

C.include "<nix/derivations.hh>"

C.include "<nix/affinity.hh>"

C.include "<nix/globals.hh>"

C.include "hercules-ci-cnix/store.hxx"

C.using "namespace nix"

newtype Store = Store (Ptr (Ref NixStore))

openStore :: IO Store
openStore =
  coerce
    [C.throwBlock| refStore* {
      refStore s = openStore();
      return new refStore(s);
    } |]

releaseStore :: Store -> IO ()
releaseStore (Store store) = [C.exp| void { delete $(refStore* store) } |]

withStore :: MonadUnliftIO m => (Store -> m a) -> m a
withStore m = do
  UnliftIO ul <- askUnliftIO
  liftIO $ withStore' $ \a -> ul (m a)

withStore' ::
  (Store -> IO r) ->
  IO r
withStore' =
  bracket openStore releaseStore

withStoreFromURI ::
  MonadUnliftIO m =>
  Text ->
  (Store -> m r) ->
  m r
withStoreFromURI storeURIText f = do
  let storeURI = encodeUtf8 storeURIText
  (UnliftIO unlift) <- askUnliftIO
  liftIO $
    bracket
      [C.throwBlock| refStore* {
        refStore s = openStore($bs-cstr:storeURI);
        return new refStore(s);
      }|]
      (\x -> [C.exp| void { delete $(refStore* x) } |])
      (unlift . f . Store)

storeUri :: MonadIO m => Store -> m ByteString
storeUri (Store store) =
  unsafeMallocBS
    [C.block| const char* {
       std::string uri = (*$(refStore* store))->getUri();
       return strdup(uri.c_str());
     } |]

ensurePath :: Store -> ByteString -> IO ()
ensurePath (Store store) path =
  [C.throwBlock| void {
    (*$(refStore* store))->ensurePath(std::string($bs-ptr:path, $bs-len:path));
  } |]

clearPathInfoCache :: Store -> IO ()
clearPathInfoCache (Store store) =
  [C.throwBlock| void {
    (*$(refStore* store))->clearPathInfoCache();
  } |]

clearSubstituterCaches :: IO ()
clearSubstituterCaches =
  [C.throwBlock| void {
    auto subs = nix::getDefaultSubstituters();
    for (auto sub : subs) {
      sub->clearPathInfoCache();
    }
  } |]

buildPaths :: Store -> [ByteString] -> IO ()
buildPaths (Store store) paths = do
  withStringsOf paths $ \pathStrings -> do
    [C.throwBlock| void {
      StringSet pathSet;
      for (auto path : *$(Strings *pathStrings)) {
        pathSet.insert(path);
      }
      (*$(refStore* store))->buildPaths(pathSet);
    }|]

buildPath :: Store -> ByteString -> IO ()
buildPath (Store store) path =
  [C.throwBlock| void {
    PathSet ps({std::string($bs-ptr:path, $bs-len:path)});
    (*$(refStore* store))->buildPaths(ps);
   } |]

getDerivation :: Store -> ByteString -> IO (ForeignPtr Derivation)
getDerivation (Store store) path = do
  ptr <-
    [C.throwBlock| Derivation *{
      return new Derivation(
          (*$(refStore* store))->derivationFromPath(std::string($bs-ptr:path, $bs-len:path))
        );
    } |]
  newForeignPtr finalizeDerivation ptr

-- Useful for testingg
getDerivationFromFile :: ByteString -> IO (ForeignPtr Derivation)
getDerivationFromFile path = do
  ptr <-
    [C.throwBlock| Derivation *{
      return new Derivation(
          readDerivation(std::string($bs-ptr:path, $bs-len:path))
        );
    } |]
  newForeignPtr finalizeDerivation ptr

finalizeDerivation :: FinalizerPtr Derivation
{-# NOINLINE finalizeDerivation #-}
finalizeDerivation =
  unsafePerformIO
    [C.exp|
    void (*)(Derivation *) {
      [](Derivation *v) {
        delete v;
      }
    } |]

-- | Throws when missing
getDerivationOutputPath :: ForeignPtr Derivation -> ByteString -> IO ByteString
getDerivationOutputPath fd outputName = withForeignPtr fd $ \d ->
  [C.throwBlock|
    const char *{
      std::string outputName($bs-ptr:outputName, $bs-len:outputName);
      Derivation *d = $(Derivation *d);
      return strdup(d->outputs.at(outputName).path.c_str());
    }
  |]
    >>= BS.unsafePackMallocCString

data DerivationOutput = DerivationOutput
  { derivationOutputName :: !ByteString,
    derivationOutputPath :: !ByteString,
    derivationOutputHashAlgo :: !ByteString,
    derivationOutputHash :: !ByteString
  }

getDerivationOutputs :: ForeignPtr Derivation -> IO [DerivationOutput]
getDerivationOutputs derivation =
  bracket
    [C.exp| DerivationOutputsIterator* {
      new DerivationOutputsIterator($fptr-ptr:(Derivation *derivation)->outputs.begin())
    }|]
    deleteDerivationOutputsIterator
    $ \i -> fix $ \continue -> do
      isEnd <- (0 /=) <$> [C.exp| bool { *$(DerivationOutputsIterator *i) == $fptr-ptr:(Derivation *derivation)->outputs.end() }|]
      if isEnd
        then pure []
        else do
          name <- [C.exp| const char*{ strdup((*$(DerivationOutputsIterator *i))->first.c_str()) }|] >>= BS.unsafePackMallocCString
          path <- [C.exp| const char*{ strdup((*$(DerivationOutputsIterator *i))->second.path.c_str()) }|] >>= BS.unsafePackMallocCString
          hash_ <- [C.exp| const char*{ strdup((*$(DerivationOutputsIterator *i))->second.hash.c_str()) }|] >>= BS.unsafePackMallocCString
          hashAlgo <- [C.exp| const char*{ strdup((*$(DerivationOutputsIterator *i))->second.hashAlgo.c_str()) }|] >>= BS.unsafePackMallocCString
          [C.block| void { (*$(DerivationOutputsIterator *i))++; }|]
          (DerivationOutput name path hashAlgo hash_ :) <$> continue

deleteDerivationOutputsIterator :: Ptr DerivationOutputsIterator -> IO ()
deleteDerivationOutputsIterator a = [C.block| void { delete $(DerivationOutputsIterator *a); }|]

getDerivationPlatform :: ForeignPtr Derivation -> IO ByteString
getDerivationPlatform derivation =
  unsafeMallocBS
    [C.exp| const char* {
       strdup($fptr-ptr:(Derivation *derivation)->platform.c_str())
     } |]

getDerivationBuilder :: ForeignPtr Derivation -> IO ByteString
getDerivationBuilder derivation =
  unsafeMallocBS
    [C.exp| const char* {
       strdup($fptr-ptr:(Derivation *derivation)->builder.c_str())
     } |]

getDerivationArguments :: ForeignPtr Derivation -> IO [ByteString]
getDerivationArguments derivation =
  bracket
    [C.throwBlock| Strings* {
      Strings *r = new Strings();
      for (auto i : $fptr-ptr:(Derivation *derivation)->args) {
        r->push_back(i);
      }
      return r;
    }|]
    deleteStrings
    toByteStrings

getDerivationSources :: ForeignPtr Derivation -> IO [ByteString]
getDerivationSources derivation =
  bracket
    [C.throwBlock| Strings* {
      Strings *r = new Strings();
      for (auto i : $fptr-ptr:(Derivation *derivation)->inputSrcs) {
        r->push_back(i);
      }
      return r;
    }|]
    deleteStrings
    toByteStrings

getDerivationInputs :: ForeignPtr Derivation -> IO [(ByteString, [ByteString])]
getDerivationInputs derivation =
  bracket
    [C.exp| DerivationInputsIterator* {
      new DerivationInputsIterator($fptr-ptr:(Derivation *derivation)->inputDrvs.begin())
    }|]
    deleteDerivationInputsIterator
    $ \i -> fix $ \continue -> do
      isEnd <- (0 /=) <$> [C.exp| bool { *$(DerivationInputsIterator *i) == $fptr-ptr:(Derivation *derivation)->inputDrvs.end() }|]
      if isEnd
        then pure []
        else do
          name <- [C.exp| const char*{ strdup((*$(DerivationInputsIterator *i))->first.c_str()) }|] >>= BS.unsafePackMallocCString
          outs <-
            bracket
              [C.block| Strings*{ 
                Strings *r = new Strings();
                for (auto i : (*$(DerivationInputsIterator *i))->second) {
                  r->push_back(i);
                }
                return r;
              }|]
              deleteStrings
              toByteStrings
          [C.block| void { (*$(DerivationInputsIterator *i))++; }|]
          ((name, outs) :) <$> continue

deleteDerivationInputsIterator :: Ptr DerivationInputsIterator -> IO ()
deleteDerivationInputsIterator a = [C.block| void { delete $(DerivationInputsIterator *a); }|]

getDerivationEnv :: ForeignPtr Derivation -> IO (Map ByteString ByteString)
getDerivationEnv derivation =
  [C.exp| StringPairs* { &($fptr-ptr:(Derivation *derivation)->env) }|]
    >>= toByteStringMap

getDerivationOutputNames :: ForeignPtr Derivation -> IO [ByteString]
getDerivationOutputNames derivation =
  bracket
    [C.throwBlock| Strings* {
      Strings *r = new Strings();
      for (auto i : $fptr-ptr:(Derivation *derivation)->outputs) {
        r->push_back(i.first);
      }
      return r;
    }|]
    deleteStrings
    toByteStrings

deleteStringPairs :: Ptr StringPairs -> IO ()
deleteStringPairs s = [C.block| void { delete $(StringPairs *s); }|]

deleteStrings :: Ptr Strings -> IO ()
deleteStrings s = [C.block| void { delete $(Strings *s); }|]

finalizeStrings :: FinalizerPtr Strings
{-# NOINLINE finalizeStrings #-}
finalizeStrings =
  unsafePerformIO
    [C.exp|
    void (*)(Strings *) {
      [](Strings *v) {
        delete v;
      }
    } |]

getStringsLength :: Ptr Strings -> IO C.CSize
getStringsLength strings = [C.exp| size_t { $(Strings *strings)->size() }|]

toByteStrings :: Ptr Strings -> IO [ByteString]
toByteStrings strings = do
  i <- [C.exp| StringsIterator *{ new StringsIterator($(Strings *strings)->begin()) } |]
  fix $ \go -> do
    isEnd <- (0 /=) <$> [C.exp| bool { *$(StringsIterator *i) == $(Strings *strings)->end() }|]
    if isEnd
      then pure []
      else do
        s <- [C.exp| const char*{ strdup((*$(StringsIterator *i))->c_str()) }|]
        bs <- BS.unsafePackMallocCString s
        [C.block| void { (*$(StringsIterator *i))++; }|]
        (bs :) <$> go

toByteStringMap :: Ptr StringPairs -> IO (Map ByteString ByteString)
toByteStringMap strings =
  M.fromList <$> do
    i <- [C.exp| StringPairsIterator *{ new StringPairsIterator($(StringPairs *strings)->begin()) } |]
    fix $ \go -> do
      isEnd <- (0 /=) <$> [C.exp| bool { *$(StringPairsIterator *i) == $(StringPairs *strings)->end() }|]
      if isEnd
        then pure []
        else do
          k <- [C.exp| const char*{ strdup((*$(StringPairsIterator *i))->first.c_str()) }|]
          v <- [C.exp| const char*{ strdup((*$(StringPairsIterator *i))->second.c_str()) }|]
          bk <- BS.unsafePackMallocCString k
          bv <- BS.unsafePackMallocCString v
          [C.block| void { (*$(StringPairsIterator *i))++; }|]
          ((bk, bv) :) <$> go

withStrings :: (Ptr Strings -> IO a) -> IO a
withStrings =
  bracket
    [C.exp| Strings *{ new Strings() }|]
    (\sp -> [C.block| void { delete $(Strings *sp); }|])

withStringsOf :: [ByteString] -> (Ptr Strings -> IO a) -> IO a
withStringsOf paths f =
  withStrings \strings -> do
    for_ paths (pushString strings)
    f strings

pushString :: Ptr Strings -> ByteString -> IO ()
pushString strings s =
  [C.block| void { $(Strings *strings)->push_back($bs-cstr:s); }|]

copyClosure :: Store -> Store -> [ByteString] -> IO ()
copyClosure (Store src) (Store dest) paths = do
  withStringsOf paths $ \pathStrings -> do
    [C.throwBlock| void {
      StringSet pathSet;
      for (auto path : *$(Strings *pathStrings)) {
        pathSet.insert(path);
      }
      nix::copyClosure(*$(refStore* src), *$(refStore* dest), pathSet);
    }|]

parseSecretKey :: ByteString -> IO (ForeignPtr SecretKey)
parseSecretKey bs =
  [C.throwBlock| SecretKey* {
    return new SecretKey($bs-cstr:bs);
  }|]
    >>= newForeignPtr finalizeSecretKey

finalizeSecretKey :: FinalizerPtr SecretKey
{-# NOINLINE finalizeSecretKey #-}
finalizeSecretKey =
  unsafePerformIO
    [C.exp|
    void (*)(SecretKey *) {
      [](SecretKey *v) {
        delete v;
      }
    } |]

signPath ::
  Store ->
  -- | Secret signing key
  Ptr SecretKey ->
  -- | Store path
  ByteString ->
  -- | False if the signature was already present, True if the signature was added
  IO Bool
signPath (Store store) secretKey path =
  (== 1) <$> do
    [C.throwBlock| int {
    nix::ref<nix::Store> store = *$(refStore *store);
    std::string storePath($bs-cstr:path);
    auto currentInfo = store->queryPathInfo(storePath);

    auto info2(*currentInfo);
    info2.sigs.clear();
    info2.sign(*$(SecretKey *secretKey));
    assert(!info2.sigs.empty());
    auto sig = *info2.sigs.begin();

    if (currentInfo->sigs.count(sig)) {
      return 0;
    } else {
      store->addSignatures(storePath, info2.sigs);
      return 1;
    }
  }|]

----- PathSet -----
newtype PathSet = PathSet (ForeignPtr (C.StdSet C.StdString))

finalizePathSet :: FinalizerPtr C.PathSet
{-# NOINLINE finalizePathSet #-}
finalizePathSet =
  unsafePerformIO
    [C.exp|
  void (*)(PathSet *) {
    [](PathSet *v){
      delete v;
    }
  } |]

newEmptyPathSet :: IO PathSet
newEmptyPathSet = do
  ptr <- [C.exp| PathSet *{ new PathSet() }|]
  fptr <- newForeignPtr finalizePathSet ptr
  pure $ PathSet fptr

addToPathSet :: ByteString -> PathSet -> IO ()
addToPathSet bs pathSet_ = withPathSet pathSet_ $ \pathSet ->
  [C.throwBlock| void { 
    $(PathSet *pathSet)->insert(std::string($bs-ptr:bs, $bs-len:bs));
  }|]

withPathSet :: PathSet -> (Ptr C.PathSet -> IO b) -> IO b
withPathSet (PathSet pathSetFptr) = withForeignPtr pathSetFptr

traversePathSet :: forall a. (ByteString -> IO a) -> PathSet -> IO [a]
traversePathSet f pathSet_ = withPathSet pathSet_ $ \pathSet -> do
  i <- [C.exp| PathSetIterator *{ new PathSetIterator($(PathSet *pathSet)->begin()) }|]
  end <- [C.exp| PathSetIterator *{ new PathSetIterator ($(PathSet *pathSet)->end()) }|]
  let cleanup =
        [C.throwBlock| void {
          delete $(PathSetIterator *i);
          delete $(PathSetIterator *end);
        }|]
  flip finally cleanup $
    let go :: ([a] -> [a]) -> IO [a]
        go acc = do
          isDone <-
            [C.exp| int {
            *$(PathSetIterator *i) == *$(PathSetIterator *end)
          }|]
          if isDone /= 0
            then pure $ acc []
            else do
              somePath <- unsafePackMallocCString =<< [C.exp| const char *{ strdup((*$(PathSetIterator *i))->c_str()) } |]
              a <- f somePath
              [C.throwBlock| void { (*$(PathSetIterator *i))++; } |]
              go (acc . (a :))
     in go identity

-- | Follow symlinks to the store and chop off the parts after the top-level store name
followLinksToStorePath :: Store -> ByteString -> IO ByteString
followLinksToStorePath (Store store) bs =
  unsafePackMallocCString
    =<< [C.throwBlock| const char *{
    return strdup((*$(refStore* store))->followLinksToStorePath(std::string($bs-ptr:bs, $bs-len:bs)).c_str());
  }|]

queryPathInfo ::
  Store ->
  -- | Exact store path, not a subpath
  ByteString ->
  -- | ValidPathInfo or exception
  IO (ForeignPtr (Ref ValidPathInfo))
queryPathInfo (Store store) path = do
  vpi <-
    [C.throwBlock| refValidPathInfo*
      {
        return new refValidPathInfo((*$(refStore* store))->queryPathInfo($bs-cstr:path));
      } |]
  newForeignPtr finalizeRefValidPathInfo vpi

finalizeRefValidPathInfo :: FinalizerPtr (Ref ValidPathInfo)
{-# NOINLINE finalizeRefValidPathInfo #-}
finalizeRefValidPathInfo =
  unsafePerformIO
    [C.exp|
  void (*)(refValidPathInfo *) {
    [](refValidPathInfo *v){ delete v; }
  } |]

-- | The narSize field of a ValidPathInfo struct. Source: store-api.hh
validPathInfoNarSize :: ForeignPtr (Ref ValidPathInfo) -> Int64
validPathInfoNarSize vpi =
  fromIntegral $
    toInteger
      [C.pure| long
        { (*$fptr-ptr:(refValidPathInfo* vpi))->narSize }
      |]

-- | Copy the narHash field of a ValidPathInfo struct. Source: store-api.hh
validPathInfoNarHash :: ForeignPtr (Ref ValidPathInfo) -> IO ByteString
validPathInfoNarHash vpi =
  unsafePackMallocCString
    =<< [C.exp| const char
        *{ strdup((*$fptr-ptr:(refValidPathInfo* vpi))->narHash.to_string().c_str()) }
      |]

-- | Deriver field of a ValidPathInfo struct. Source: store-api.hh
--
-- Returns 'unknownDeriver' when missing.
validPathInfoDeriver :: ForeignPtr (Ref ValidPathInfo) -> IO ByteString
validPathInfoDeriver vpi =
  unsafePackMallocCString
    =<< [C.throwBlock| const char*
        {
          std::optional<Path> deriver = (*$fptr-ptr:(refValidPathInfo* vpi))->deriver;
          return strdup((deriver == "" ? "unknown-deriver" : deriver->c_str()));
        }
      |]

-- | String constant representing the case when the deriver of a store path does
-- not exist or is not known. Value: @unknown-deriver@
unknownDeriver :: Text
unknownDeriver = "unknown-deriver"

-- | References field of a ValidPathInfo struct. Source: store-api.hh
validPathInfoReferences :: ForeignPtr (Ref ValidPathInfo) -> IO PathSet
validPathInfoReferences vpi = do
  ptr <-
    [C.exp| const PathSet*
            { new PathSet((*$fptr-ptr:(refValidPathInfo* vpi))->references) }
        |]
  fptr <- newForeignPtr finalizePathSet ptr
  pure $ PathSet fptr

----- computeFSClosure -----
data ClosureParams = ClosureParams
  { flipDirection :: Bool,
    includeOutputs :: Bool,
    includeDerivers :: Bool
  }

defaultClosureParams :: ClosureParams
defaultClosureParams =
  ClosureParams
    { flipDirection = False,
      includeOutputs = False,
      includeDerivers = False
    }

computeFSClosure :: Store -> ClosureParams -> PathSet -> IO PathSet
computeFSClosure (Store store) params startingSet_ = withPathSet startingSet_ $ \startingSet -> do
  let countTrue :: Bool -> C.CInt
      countTrue True = 1
      countTrue False = 0
      flipDir = countTrue $ flipDirection params
      inclOut = countTrue $ includeOutputs params
      inclDrv = countTrue $ includeDerivers params
  ps <-
    [C.throwBlock| PathSet* {
             PathSet *r = new PathSet();
             (*$(refStore* store))->computeFSClosure(*$(PathSet *startingSet), *r, $(int flipDir), $(int inclOut), $(int inclDrv));
             return r;
           } |]
  fp <- newForeignPtr finalizePathSet ps
  pure $ PathSet fp
