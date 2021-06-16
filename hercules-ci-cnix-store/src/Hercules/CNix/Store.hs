{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.ByteString.Unsafe (unsafePackMallocCString)
import qualified Data.ByteString.Unsafe as BS
import Data.Coerce (coerce)
import qualified Data.Map as M
import Foreign (alloca, free, nullPtr)
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Storable (peek)
import Hercules.CNix.Encapsulation (HasEncapsulation (..))
import Hercules.CNix.Std.Set (StdSet, stdSetCtx)
import qualified Hercules.CNix.Std.Set as Std.Set
import Hercules.CNix.Std.String (stdStringCtx)
import qualified Hercules.CNix.Std.String as Std.String
import Hercules.CNix.Std.Vector
import qualified Hercules.CNix.Std.Vector as Std.Vector
import Hercules.CNix.Store.Context
  ( DerivationInputsIterator,
    DerivationOutputsIterator,
    NixStore,
    NixStorePath,
    Ref,
    SecretKey,
    StringPairs,
    Strings,
    ValidPathInfo,
    context,
    unsafeMallocBS,
  )
import qualified Hercules.CNix.Store.Context as C hiding (context)
import Hercules.CNix.Store.Instances ()
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import Protolude
import System.IO.Unsafe (unsafePerformIO)
import qualified Prelude

C.context (context <> stdVectorCtx <> stdSetCtx <> stdStringCtx)

C.include "<cstring>"

C.include "<nix/config.h>"

C.include "<nix/shared.hh>"

C.include "<nix/store-api.hh>"

C.include "<nix/get-drvs.hh>"

C.include "<nix/derivations.hh>"

C.include "<nix/affinity.hh>"

C.include "<nix/globals.hh>"

C.include "<nix/path.hh>"

C.include "<variant>"

C.include "<nix/worker-protocol.hh>"

#ifdef NIX_2_4
C.include "<nix/path-with-outputs.hh>"
#endif

C.include "hercules-ci-cnix/store.hxx"

C.include "nix-compat.hh"

C.using "namespace nix"

#ifndef NIX_2_4
C.using "namespace compat::nix"
#endif

forNonNull :: Applicative m => Ptr a -> (Ptr a -> m b) -> m (Maybe b)
forNonNull = flip traverseNonNull

traverseNonNull :: Applicative m => (Ptr a -> m b) -> Ptr a -> m (Maybe b)
traverseNonNull f p = if p == nullPtr then pure Nothing else Just <$> f p

newtype Store = Store (Ptr (Ref NixStore))

openStore :: IO Store
openStore =
  coerce
    [C.throwBlock| refStore * {
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

-- | Usually @"/nix/store"@
storeDir :: MonadIO m => Store -> m ByteString
storeDir (Store store) =
  unsafeMallocBS
    [C.block| const char* {
       std::string uri = (*$(refStore* store))->storeDir;
       return strdup(uri.c_str());
     } |]

getStoreProtocolVersion :: Store -> IO Int
getStoreProtocolVersion (Store store) =
  fromIntegral
    <$> [C.throwBlock| int {
       Store &store = **$(refStore* store);
       return store.getProtocol();
     } |]

getClientProtocolVersion :: IO Int
getClientProtocolVersion =
  fromIntegral
    <$> [C.throwBlock| int {
       return PROTOCOL_VERSION;
     } |]

-- | Store-agnostic store path representation: hash and name. Does not have a storedir or subpath inside the store path.
newtype StorePath = StorePath (ForeignPtr NixStorePath)

instance HasEncapsulation NixStorePath StorePath where
  moveToForeignPtrWrapper = moveStorePath

finalizeStorePath :: FinalizerPtr NixStorePath
{-# NOINLINE finalizeStorePath #-}
finalizeStorePath =
  unsafePerformIO
    [C.exp|
      void (*)(nix::StorePath *) {
        [](StorePath *v) {
          delete v;
        }
      }
    |]

-- | Move ownership of a Ptr NixStorePath into 'StorePath'
moveStorePath :: Ptr NixStorePath -> IO StorePath
moveStorePath x = StorePath <$> newForeignPtr finalizeStorePath x

-- | Move ownership of a Ptr NixStorePath into 'StorePath'
moveStorePathMaybe :: Ptr NixStorePath -> IO (Maybe StorePath)
moveStorePathMaybe = traverseNonNull $ fmap StorePath . newForeignPtr finalizeStorePath

instance Prelude.Show StorePath where
  show storePath = unsafePerformIO do
    bs <-
      BS.unsafePackMallocCString
        =<< [C.block| const char* {
          std::string s($fptr-ptr:(nix::StorePath *storePath)->to_string());
          return strdup(s.c_str());
        }|]
    pure $ toS $ decodeUtf8With lenientDecode bs

instance Eq StorePath where
  a == b = compare a b == EQ

-- FIXME
instance Ord StorePath where
  compare (StorePath a) (StorePath b) =
    compare
      0
      [C.pure| int {
        $fptr-ptr:(nix::StorePath *a)->to_string().compare($fptr-ptr:(nix::StorePath *b)->to_string())
      }|]

-- | Create 'StorePath' from hash and name.
--
-- Throws C++ `BadStorePath` exception when invalid.
parseStorePathBaseName :: ByteString -> IO StorePath
parseStorePathBaseName bs =
  moveStorePath
    =<< [C.throwBlock| nix::StorePath *{
      return new StorePath(std::string($bs-ptr:bs, $bs-len:bs));
    }|]

-- | Parse a complete store path including storeDir into a 'StorePath'.
--
-- Throws C++ `BadStorePath` exception when invalid.
parseStorePath :: Store -> ByteString -> IO StorePath
#ifdef NIX_2_4
parseStorePath (Store store) bs =
  moveStorePath
    =<< [C.throwBlock| nix::StorePath *{
      return new StorePath(std::move((*$(refStore* store))->parseStorePath(std::string($bs-ptr:bs, $bs-len:bs))));
    }|]
#else
parseStorePath (Store store) bs =
  moveStorePath
    =<< [C.throwBlock| nix::StorePath *{
      auto p = canonPath(std::string($bs-ptr:bs, $bs-len:bs));
      if (dirOf(p) != (*$(refStore* store))->storeDir)
        throw Error("path '%s' is not in the Nix store", p);
      return new StorePath(baseNameOf(p));
    }|]
#endif

getStorePathBaseName :: StorePath -> IO ByteString
getStorePathBaseName (StorePath sp) = do
  BS.unsafePackMallocCString
    =<< [C.block| const char *{
      std::string s($fptr-ptr:(nix::StorePath *sp)->to_string());
      return strdup(s.c_str());
    }|]

getStorePathHash :: StorePath -> IO ByteString
getStorePathHash (StorePath sp) = do
  BS.unsafePackMallocCString
    =<< [C.block| const char *{
      std::string s($fptr-ptr:(nix::StorePath *sp)->hashPart());
      return strdup(s.c_str());
    }|]

storePathToPath :: Store -> StorePath -> IO ByteString
#ifdef NIX_2_4
storePathToPath (Store store) (StorePath sp) =
  BS.unsafePackMallocCString
    =<< [C.block| const char *{
      Store & store = **$(refStore* store);
      StorePath &sp = *$fptr-ptr:(nix::StorePath *sp);
      std::string s(store.printStorePath(sp));
      return strdup(s.c_str());
    }|]
#else
storePathToPath (Store store) (StorePath sp) =
  BS.unsafePackMallocCString
    =<< [C.block| const char *{
      Store & store = **$(refStore* store);
      StorePath &sp = *$fptr-ptr:(nix::StorePath *sp);
      std::string s(printPath23(store, sp));
      return strdup(s.c_str());
    }|]
#endif

ensurePath :: Store -> StorePath -> IO ()
ensurePath (Store store) (StorePath storePath) =
  [C.throwBlock| void {
    Store &store = **$(refStore* store);
    StorePath &storePath = *$fptr-ptr:(nix::StorePath *storePath);
    store.ensurePath(printPath23(store, storePath));
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

newtype StorePathWithOutputs = StorePathWithOutputs (ForeignPtr C.NixStorePathWithOutputs)

instance HasEncapsulation C.NixStorePathWithOutputs StorePathWithOutputs where
  moveToForeignPtrWrapper x = StorePathWithOutputs <$> newForeignPtr finalizeStorePathWithOutputs x

finalizeStorePathWithOutputs :: FinalizerPtr C.NixStorePathWithOutputs
{-# NOINLINE finalizeStorePathWithOutputs #-}
finalizeStorePathWithOutputs =
  unsafePerformIO
    [C.exp|
      void (*)(nix::StorePathWithOutputs *) {
        [](StorePathWithOutputs *v) {
          delete v;
        }
      }
    |]

newStorePathWithOutputs :: StorePath -> [ByteString] -> IO StorePathWithOutputs
newStorePathWithOutputs storePath outputs = do
  set <- Std.Set.new
  for_ outputs (\o -> Std.String.withString o (Std.Set.insertP set))
  moveToForeignPtrWrapper
    =<< [C.exp| nix::StorePathWithOutputs * {
    new StorePathWithOutputs {*$fptr-ptr:(nix::StorePath *storePath), *$fptr-ptr:(std::set<std::string>* set)}
  }|]

getStorePath :: StorePathWithOutputs -> IO StorePath
getStorePath swo = mask_ do
  moveToForeignPtrWrapper
    =<< [C.exp| nix::StorePath * {
    new StorePath($fptr-ptr:(nix::StorePathWithOutputs *swo)->path)
  }|]

getOutputs :: StorePathWithOutputs -> IO [ByteString]
getOutputs swo = mask_ do
  traverse Std.String.moveToByteString =<< toListP =<< moveToForeignPtrWrapper
    =<< [C.throwBlock| std::vector<std::string>* {
      auto r = new std::vector<std::string>();
      for (auto s : $fptr-ptr:(nix::StorePathWithOutputs *swo)->outputs)
        r->push_back(s);
      return r;
    }|]

buildPaths :: Store -> StdVector C.NixStorePathWithOutputs -> IO ()
buildPaths (Store store) (StdVector paths) = do
#ifdef NIX_2_4
  [C.throwBlock| void {
    Store &store = **$(refStore* store);
    std::vector<StorePathWithOutputs> &paths = *$fptr-ptr:(std::vector<nix::StorePathWithOutputs>* paths);
    store.buildPaths(toDerivedPaths(paths));
  }|]
#else
  [C.throwBlock| void {
    Store &store = **$(refStore* store);
    std::vector<StorePathWithOutputs> &paths = *$fptr-ptr:(std::vector<nix::StorePathWithOutputs>* paths);
    store.buildPaths(printPathSet23(store, paths));
  }|]
#endif

buildPath :: Store -> StorePathWithOutputs -> IO ()
buildPath store spwo = do
  buildPaths store =<< Std.Vector.fromListFP [spwo]


newtype Derivation = Derivation (ForeignPtr C.Derivation)

instance HasEncapsulation C.Derivation Derivation where
  moveToForeignPtrWrapper = fmap Derivation . newForeignPtr finalizeDerivation

finalizeDerivation :: FinalizerPtr C.Derivation
{-# NOINLINE finalizeDerivation #-}
finalizeDerivation =
  unsafePerformIO
    [C.exp|
    void (*)(Derivation *) {
      [](Derivation *v) {
        delete v;
      }
    } |]

getDerivation :: Store -> StorePath -> IO Derivation
getDerivation (Store store) (StorePath spwo) = do
  moveToForeignPtrWrapper
    =<< [C.throwBlock| Derivation *{
      Store &store = **$(refStore* store);
      return new Derivation(
          store.derivationFromPath(printPath23(store, *$fptr-ptr:(nix::StorePath *spwo)))
        );
    } |]

-- Useful for testing
getDerivationFromString ::
  Store ->
  -- | Derivation name (store path name with ".drv" extension removed)
  ByteString ->
  -- | Contents
  ByteString ->
  IO Derivation
#ifdef NIX_2_4
getDerivationFromString (Store store) name contents = do
  moveToForeignPtrWrapper
    =<< [C.throwBlock| Derivation *{
      Store &store = **$(refStore* store);
      std::string name($bs-ptr:name, $bs-len:name);
      return new Derivation(parseDerivation(store, std::string($bs-ptr:contents, $bs-len:contents), name));
    }|]
#else
getDerivationFromString _store name contents = do
  moveToForeignPtrWrapper =<< [C.throwBlock| Derivation *{
      std::string name($bs-ptr:name, $bs-len:name);
      std::string contents($bs-ptr:contents, $bs-len:contents);
      AutoDelete tmpDir(createTempDir(), true);
      Path tmpFile = (Path) tmpDir + "/" + name;
      writeFile(tmpFile, contents, 0600);
      return new Derivation(readDerivation(tmpFile));
    }|]
#endif

getDerivationNameFromPath :: StorePath -> IO ByteString
getDerivationNameFromPath storePath =
  BS.unsafePackMallocCString

#ifdef NIX_2_4
    =<< [C.throwBlock| const char *{
      StorePath &sp = *$fptr-ptr:(nix::StorePath *storePath);
      std::string s(Derivation::nameFromPath(sp));
      return strdup(s.c_str());
    }|]
#else
    =<< [C.throwBlock| const char *{
      StorePath &sp = *$fptr-ptr:(nix::StorePath *storePath);
      std::string pathName(sp.name());
      assert(isDerivation(pathName));
      std::string drvName = std::string(pathName, 0, pathName.size() - drvExtension.size());
      return strdup(drvName.c_str());
    }|]
#endif

data DerivationOutput = DerivationOutput
  { derivationOutputName :: !ByteString,
    derivationOutputPath :: !(Maybe StorePath),
    derivationOutputDetail :: !DerivationOutputDetail
  }
  deriving (Eq, Show)

data DerivationOutputDetail
  = DerivationOutputInputAddressed StorePath
  | DerivationOutputCAFixed FixedOutputHash StorePath
  | DerivationOutputCAFloating FileIngestionMethod HashType
  | DerivationOutputDeferred
  deriving (Eq, Show)

data FixedOutputHash = FixedOutputHash !FileIngestionMethod {-# UNPACK #-} !Hash
  deriving (Eq, Show)

-- | See @content-address.hh@
data FileIngestionMethod = Flat | Recursive
  deriving (Eq, Show)

-- | See @hash.hh@
data Hash = Hash !HashType {-# UNPACK #-} !ShortByteString
  deriving (Eq, Show)

-- | See @hash.hh@
data HashType = MD5 | SHA1 | SHA256 | SHA512
  deriving (Eq, Show)

getDerivationOutputs :: Store -> ByteString -> Derivation -> IO [DerivationOutput]
getDerivationOutputs (Store store) drvName (Derivation derivation) =
  bracket
    [C.exp| DerivationOutputsIterator* {
      new DerivationOutputsIterator($fptr-ptr:(Derivation *derivation)->outputs.begin())
    }|]
    deleteDerivationOutputsIterator
    $ \i -> fix $ \continue -> do
      isEnd <- (0 /=) <$> [C.exp| bool { *$(DerivationOutputsIterator *i) == $fptr-ptr:(Derivation *derivation)->outputs.end() }|]
      if isEnd
        then pure []
        else
          ( mask_ do
              alloca \nameP -> alloca \pathP -> alloca \typP -> alloca \fimP ->
                alloca \hashTypeP -> alloca \hashValueP -> alloca \hashSizeP -> do
                  [C.throwBlock| void {
                    Store &store = **$(refStore *store);
                    std::string drvName = std::string($bs-ptr:drvName, $bs-len:drvName);
                    nix::DerivationOutputs::iterator &i = *$(DerivationOutputsIterator *i);
                    const char *&name = *$(const char **nameP);
                    int &typ = *$(int *typP);
                    StorePath *& path = *$(nix::StorePath **pathP);
                    int &fim = *$(int *fimP);
                    int &hashType = *$(int *hashTypeP);
                    char *&hashValue = *$(char **hashValueP);
                    int &hashSize = *$(int *hashSizeP);

                    std::string nameString = i->first;
                    name = strdup(nameString.c_str());
                    path = nullptr;
                    std::visit(overloaded {
                      [&](DerivationOutputInputAddressed doi) -> void {
                        typ = 0;
                        path = new StorePath(doi.path);
                      },
                      [&](DerivationOutputCAFixed dof) -> void {
                        typ = 1;
#ifdef NIX_2_4
                        path = new StorePath(dof.path(store, $fptr-ptr:(Derivation *derivation)->name, nameString));
#else
                        path = new StorePath(dof.path(store, dof.drvName, nameString));
#endif
                        switch (dof.hash.method) {
                          case nix::FileIngestionMethod::Flat:
                            fim = 0;
                            break;
                          case nix::FileIngestionMethod::Recursive:
                            fim = 1;
                            break;
                          default:
                            fim = -1;
                            break;
                        }
                        switch (dof.hash.hash.type) {
                          case htMD5: 
                            hashType = 0;
                            break;
                          case htSHA1: 
                            hashType = 1;
                            break;
                          case htSHA256: 
                            hashType = 2;
                            break;
                          case htSHA512: 
                            hashType = 3;
                            break;
                          default:
                            hashType = -1;
                            break;
                        }
                        hashSize = dof.hash.hash.hashSize;
                        hashValue = (char*)malloc(hashSize);
                        std::memcpy((void*)(hashValue),
                                    (void*)(dof.hash.hash.hash),
                                    hashSize);
                      },
                      [&](DerivationOutputCAFloating dof) -> void {
                        typ = 2;
                        switch (dof.method) {
                          case nix::FileIngestionMethod::Flat:
                            fim = 0;
                            break;
                          case nix::FileIngestionMethod::Recursive:
                            fim = 1;
                            break;
                          default:
                            fim = -1;
                            break;
                        }
                        switch (dof.hashType) {
                          case htMD5: 
                            hashType = 0;
                            break;
                          case htSHA1: 
                            hashType = 1;
                            break;
                          case htSHA256: 
                            hashType = 2;
                            break;
                          case htSHA512: 
                            hashType = 3;
                            break;
                          default:
                            hashType = -1;
                            break;
                        }
                      },
                      [&](DerivationOutputDeferred) -> void {
                        typ = 3;
                      },
#ifdef NIX_2_4
                    }, i->second.output);
#else
                    }, compatDerivationOutput(store, drvName, i->second).output);
#endif
                    i++;
                  }|]
                  name <- unsafePackMallocCString =<< peek nameP
                  path <- moveStorePathMaybe =<< peek pathP
                  typ <- peek typP
                  let getFileIngestionMethod = peek fimP <&> \case 0 -> Flat; 1 -> Recursive; _ -> panic "getDerivationOutputs: unknown fim"
                      getHashType =
                        peek hashTypeP <&> \case
                          0 -> MD5
                          1 -> SHA1
                          2 -> SHA256
                          3 -> SHA512
                          _ -> panic "getDerivationOutputs: unknown hashType"
                  detail <- case typ of
                    0 -> pure $ DerivationOutputInputAddressed (fromMaybe (panic "getDerivationOutputs: impossible DOIA path missing") path)
                    1 -> do
                      hashValue <- peek hashValueP
                      hashSize <- peek hashSizeP
                      hashString <- SBS.packCStringLen (hashValue, fromIntegral hashSize)
                      free hashValue
                      hashType <- getHashType
                      fim <- getFileIngestionMethod
                      pure $ DerivationOutputCAFixed (FixedOutputHash fim (Hash hashType hashString)) (fromMaybe (panic "getDerivationOutputs: impossible DOCF path missing") path)
                    2 -> do
                      hashType <- getHashType
                      fim <- getFileIngestionMethod
                      pure $ DerivationOutputCAFloating fim hashType
                    3 -> pure DerivationOutputDeferred
                    _ -> panic "getDerivationOutputs: impossible getDerivationOutputs typ"
                  pure
                    ( DerivationOutput
                        { derivationOutputName = name,
                          derivationOutputPath = path,
                          derivationOutputDetail = detail
                        }
                        :
                    )
          )
            <*> continue

deleteDerivationOutputsIterator :: Ptr DerivationOutputsIterator -> IO ()
deleteDerivationOutputsIterator a = [C.block| void { delete $(DerivationOutputsIterator *a); }|]

getDerivationPlatform :: Derivation -> IO ByteString
getDerivationPlatform derivation =
  unsafeMallocBS
    [C.exp| const char* {
       strdup($fptr-ptr:(Derivation *derivation)->platform.c_str())
     } |]

getDerivationBuilder :: Derivation -> IO ByteString
getDerivationBuilder derivation =
  unsafeMallocBS
    [C.exp| const char* {
       strdup($fptr-ptr:(Derivation *derivation)->builder.c_str())
     } |]

getDerivationArguments :: Derivation -> IO [ByteString]
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

getDerivationSources :: Store -> Derivation -> IO [StorePath]
getDerivationSources (Store store) derivation = mask_ do
  vec <-
    moveToForeignPtrWrapper
      =<< [C.throwBlock| std::vector<nix::StorePath*>* {
        Store &store = **$(refStore* store);
        auto r = new std::vector<StorePath *>();
        for (auto s : $fptr-ptr:(Derivation *derivation)->inputSrcs)
          r->push_back(new StorePath(parseStorePath23(store, s)));
        return r;
      }|]
  traverse moveStorePath =<< Std.Vector.toList vec

getDerivationInputs :: Store -> Derivation -> IO [(StorePath, [ByteString])]
getDerivationInputs (Store store) derivation =
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
          name <-
            [C.throwBlock| nix::StorePath *{
              Store &store = **$(refStore* store);
              return new StorePath(parseStorePath23(store, (*$(DerivationInputsIterator *i))->first));
            }|]
              >>= moveStorePath
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

getDerivationEnv :: Derivation -> IO (Map ByteString ByteString)
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

copyClosure :: Store -> Store -> [StorePath] -> IO ()
copyClosure (Store src) (Store dest) pathList = do
  (StdVector pathsVector') <- Std.Vector.fromList (pathList <&> \(StorePath c) -> unsafeForeignPtrToPtr c)
  withForeignPtr pathsVector' \pathsVector ->
    [C.throwBlock| void {
      ref<Store> src = *$(refStore* src);
      ref<Store> dest = *$(refStore* dest);
      std::vector<nix::StorePath *> &pathsVector = *$(std::vector<nix::StorePath*>* pathsVector);

      StorePathSet pathSet;
      for (auto spp : pathsVector)
        pathSet.insert(*spp);

      StorePathSet closurePaths;
      compatComputeFSClosure(*src, pathSet, closurePaths);

      nix::copyPaths(src, dest, compatPathSet(*src, closurePaths));
    }|]
  for_ pathList (\(StorePath c) -> touchForeignPtr c)

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
  StorePath ->
  -- | False if the signature was already present, True if the signature was added
  IO Bool
signPath (Store store) secretKey (StorePath path) =
  (== 1) <$> do
    [C.throwBlock| int {
    nix::ref<nix::Store> store = *$(refStore *store);
    const StorePath &storePath = *$fptr-ptr:(nix::StorePath *path);
    const SecretKey &secretKey = *$(SecretKey *secretKey);
    auto currentInfo = store->queryPathInfo(printPath23(*store, storePath));

    auto info2(*currentInfo);
    info2.sigs.clear();
#ifdef NIX_2_4
    info2.sign(*store, secretKey);
#else
    info2.sign(secretKey);
#endif
    assert(!info2.sigs.empty());
    auto sig = *info2.sigs.begin();

    if (currentInfo->sigs.count(sig)) {
      return 0;
    } else {
      store->addSignatures(printPath23(*store, storePath), info2.sigs);
      return 1;
    }
  }|]

-- | Follow symlinks to the store and chop off the parts after the top-level store name
followLinksToStorePath :: Store -> ByteString -> IO StorePath
followLinksToStorePath (Store store) bs =
  moveStorePath
    =<< [C.throwBlock| nix::StorePath *{
      Store &store = **$(refStore* store);
      std::string s = std::string($bs-ptr:bs, $bs-len:bs);
      return new StorePath(parseStorePath23(store, store.followLinksToStorePath(s)));
    }|]

queryPathInfo ::
  Store ->
  -- | Exact store path, not a subpath
  StorePath ->
  -- | ValidPathInfo or exception
  IO (ForeignPtr (Ref ValidPathInfo))
queryPathInfo (Store store) (StorePath path) = do
  vpi <-
    [C.throwBlock| refValidPathInfo* {
      Store &store = **$(refStore* store);
      StorePath &path = *$fptr-ptr:(nix::StorePath *path);
      return new refValidPathInfo(store.queryPathInfo(printPath23(store, path)));
    }|]
  newForeignPtr finalizeRefValidPathInfo vpi

finalizeRefValidPathInfo :: FinalizerPtr (Ref ValidPathInfo)
{-# NOINLINE finalizeRefValidPathInfo #-}
finalizeRefValidPathInfo =
  unsafePerformIO
    [C.exp|
      void (*)(refValidPathInfo *) {
        [](refValidPathInfo *v){ delete v; }
      }|]

-- | The narSize field of a ValidPathInfo struct. Source: path-info.hh / store-api.hh
validPathInfoNarSize :: ForeignPtr (Ref ValidPathInfo) -> Int64
validPathInfoNarSize vpi =
  fromIntegral $
    toInteger
      [C.pure| long
        { (*$fptr-ptr:(refValidPathInfo* vpi))->narSize }
      |]

-- | Copy the narHash field of a ValidPathInfo struct. Source: path-info.hh / store-api.hh
validPathInfoNarHash32 :: ForeignPtr (Ref ValidPathInfo) -> IO ByteString
validPathInfoNarHash32 vpi =
  unsafePackMallocCString
    =<< [C.block| const char *{ 
      std::string s((*$fptr-ptr:(refValidPathInfo* vpi))->narHash.to_string(nix::Base32, true));
      return strdup(s.c_str()); }
    |]

-- | Deriver field of a ValidPathInfo struct. Source: store-api.hh
--
-- Returns 'unknownDeriver' when missing.
validPathInfoDeriver :: Store -> ForeignPtr (Ref ValidPathInfo) -> IO (Maybe StorePath)
validPathInfoDeriver (Store store) vpi =
  moveStorePathMaybe
    =<< [C.throwBlock| nix::StorePath * {
      Store &store = **$(refStore* store);
      std::optional<StorePath> deriver = parseOptionalStorePath23(store, (*$fptr-ptr:(refValidPathInfo* vpi))->deriver);
      return deriver ? new StorePath(*deriver) : nullptr;
    }|]

-- | References field of a ValidPathInfo struct. Source: store-api.hh
validPathInfoReferences :: Store -> ForeignPtr (Ref ValidPathInfo) -> IO [StorePath]
validPathInfoReferences (Store store) vpi = do
  sps <-
    moveToForeignPtrWrapper
      =<< [C.throwBlock| std::vector<nix::StorePath *>* {
        Store &store = **$(refStore* store);
        auto sps = new std::vector<nix::StorePath *>();
        for (auto sp : parseStorePathSet23(store, (*$fptr-ptr:(refValidPathInfo* vpi))->references))
          sps->push_back(new StorePath(sp));
        return sps;
      }|]
  l <- Std.Vector.toList sps
  for l moveStorePath

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

computeFSClosure :: Store -> ClosureParams -> StdSet NixStorePath -> IO (StdSet NixStorePath)
computeFSClosure (Store store) params (Std.Set.StdSet startingSet) = do
  let countTrue :: Bool -> C.CInt
      countTrue True = 1
      countTrue False = 0
      flipDir = countTrue $ flipDirection params
      inclOut = countTrue $ includeOutputs params
      inclDrv = countTrue $ includeDerivers params
  ret@(Std.Set.StdSet retSet) <- Std.Set.new
  [C.throwBlock| void {
    Store &store = **$(refStore* store);
    StorePathSet &ret = *$fptr-ptr:(std::set<nix::StorePath>* retSet);
    compatComputeFSClosure(store, *$fptr-ptr:(std::set<nix::StorePath>* startingSet), ret,
      $(int flipDir), $(int inclOut), $(int inclDrv));
  }|]
  pure ret

withPtr' :: (Coercible a' (ForeignPtr a)) => a' -> (Ptr a -> IO b) -> IO b
withPtr' p = withForeignPtr (coerce p)
