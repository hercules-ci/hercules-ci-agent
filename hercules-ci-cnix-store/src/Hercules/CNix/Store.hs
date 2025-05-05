{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
-- 'Delete' and 'Finalizer' instances are necessarily orphan instances due to TH staging restrictions.
{-# OPTIONS_GHC -Wno-orphans #-}
#ifdef __GHCIDE__
# define NIX_IS_AT_LEAST(mm,m,p) 1
#endif

module Hercules.CNix.Store
  ( -- * Opening a 'Store'
    Store (..),
    openStore,
    releaseStore,
    withStore,
    withStore',
    withStoreFromURI,

    -- * Store properties
    storeUri,
    storeDir,
    getStoreProtocolVersion,
    getClientProtocolVersion,

    -- * Store paths
    StorePath (..),
    parseStorePathBaseName,
    parseStorePath,
    followLinksToStorePath,
    storePathToPath,
    getStorePathBaseName,
    getStorePathHash,

    -- * Store objects
    isValidPath,
    queryPathInfo,
    queryPathInfoFromClientCache,
    ValidPathInfo,
    validPathInfoNarSize,
    validPathInfoNarHash32,
    validPathInfoDeriver,
    validPathInfoDeriver',
    validPathInfoReferences,
    validPathInfoReferences',

    computeFSClosure,
    ClosureParams(..),
    defaultClosureParams,

    -- * Realisation
    ensurePath,
    buildPaths,
    buildPath,

    -- * Garbage collection
    addTemporaryRoot,

    -- * In-memory cache control
    clearPathInfoCache,
    clearSubstituterCaches,

    -- * Derivation references
    StorePathWithOutputs (..),
    newStorePathWithOutputs,
    getStorePath,
    getOutputs,

    -- * Derivations
    Derivation (..),
    getDerivation,
    getDerivationFromString,
    getDerivationNameFromPath,
    getDerivationPlatform,
    getDerivationBuilder,
    getDerivationArguments,
    getDerivationEnv,
    getDerivationSources,
    getDerivationSources',
    getDerivationInputs,
    getDerivationInputs',

    getDerivationOutputNames,
    DerivationOutput (..),
    DerivationOutputDetail (..),
    FixedOutputHash (..),
    FileIngestionMethod (..),
    getDerivationOutputs,

    -- * Copying
    copyClosure,

    -- * Signing
    SecretKey,
    parseSecretKey,
    signPath,

    -- * Hashes
    Hash (..),
    HashType (..),

    -- * Utilities
    --
    -- To be moved and deprecated
    Strings,
    withStrings,
    withStringsOf,
    pushString,
    getStringsLength,
    toByteStrings,
    toByteStringMap,

    -- * Deprecated
    forNonNull,
    traverseNonNull,
    deleteDerivationInputsIterator,
    deleteDerivationOutputsIterator,
    deleteStringPairs,
    deleteStrings,
    finalizeDerivation,
    finalizeRefValidPathInfo,
    finalizeSecretKey,
    finalizeStorePath,
    finalizeStorePathWithOutputs,
    finalizeStrings,
    moveStorePath,
    moveStorePathMaybe,
    unsafeMallocBS,
    withPtr',
    DerivationInputsIterator,
    DerivationOutputsIterator,
    NixStore,
    NixStorePath,
    Ref,
    StringPairs,

    -- * Re-exports
    Hercules.CNix.Store.Context.context,
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
import Foreign (alloca, free)
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Storable (peek)
import Hercules.CNix.Encapsulation (HasEncapsulation (..), nullableMoveToForeignPtrWrapper)
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
import Hercules.CNix.Memory (Delete(delete), Finalizer (finalizer), withDelete, toForeignPtr)
import qualified Hercules.CNix.Memory
import Hercules.CNix.Store.Instances ()
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Protolude
import System.IO.Unsafe (unsafePerformIO)
import qualified Prelude

C.context (context <> stdVectorCtx <> stdSetCtx <> stdStringCtx)

C.include "<cstring>"

C.include "<variant>"

#if NIX_IS_AT_LEAST(2, 28, 0)

C.include "<nix/main/shared.hh>"
C.include "<nix/store/store-api.hh>"
C.include "<nix/store/derivations.hh>"
C.include "<nix/store/globals.hh>"
C.include "<nix/store/path.hh>"
C.include "<nix/store/worker-protocol.hh>"
C.include "<nix/store/path-with-outputs.hh>"
C.include "<nix/util/signals.hh>"

#else

C.include "<nix/config.h>"

C.include "<nix/shared.hh>"

C.include "<nix/store-api.hh>"

C.include "<nix/get-drvs.hh>"

C.include "<nix/derivations.hh>"

C.include "<nix/globals.hh>"

C.include "<nix/path.hh>"

C.include "<nix/worker-protocol.hh>"

C.include "<nix/path-with-outputs.hh>"

C.include "<nix/hash.hh>"

#  if NIX_IS_AT_LEAST(2,19,0)

C.include "<nix/signals.hh>"

C.include "<nix/hash.hh>"

#  endif

#  if ! NIX_IS_AT_LEAST(2,20,0)

C.include "<nix/nar-info-disk-cache.hh>"

#  endif
#endif

C.include "hercules-ci-cnix/store.hxx"

C.include "hercules-ci-cnix/string.hxx"

C.using "namespace nix"

C.using "namespace hercules_ci_cnix"

{-# DEPRECATED forNonNull "Use 'Hercules.CNix.Memory.forNonNull' instead" #-}
forNonNull :: Ptr a -> (Ptr a -> IO b) -> IO (Maybe b)
forNonNull = Hercules.CNix.Memory.forNonNull

{-# DEPRECATED traverseNonNull "Use 'Hercules.CNix.Memory.traverseNonNull' instead" #-}
traverseNonNull :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
traverseNonNull = Hercules.CNix.Memory.traverseNonNull

newtype Store = Store (Ptr (Ref NixStore))

instance Delete (Ref NixStore) where
  delete store = [C.exp| void { delete $(refStore* store) } |]

openStore :: IO Store
openStore =
  coerce
    [C.throwBlock| refStore * {
      refStore s = openStore();
      return new refStore(s);
    } |]

releaseStore :: Store -> IO ()
releaseStore (Store store) = delete store

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
    withDelete
      [C.throwBlock| refStore* {
        refStore s = openStore($bs-cstr:storeURI);
        return new refStore(s);
      }|]
      (unlift . f . Store)

storeUri :: MonadIO m => Store -> m ByteString
storeUri (Store store) = liftIO do
  BS.unsafePackMallocCString =<<
    [C.block| const char* {
       std::string uri = (*$(refStore* store))->getUri();
       return stringdup(uri);
     } |]

-- | Usually @"/nix/store"@
storeDir :: MonadIO m => Store -> m ByteString
storeDir (Store store) = liftIO do
  BS.unsafePackMallocCString =<<
    [C.block| const char* {
       std::string uri = (*$(refStore* store))->storeDir;
       return stringdup(uri);
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

instance HasEncapsulation NixStorePath StorePath

instance Finalizer NixStorePath where
  finalizer = finalizeStorePath -- must be CAF

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
{-# DEPRECATED finalizeStorePath "Use 'finalizer' instead" #-}

-- | Move ownership of a Ptr NixStorePath into 'StorePath'
moveStorePath :: Ptr NixStorePath -> IO StorePath
moveStorePath = moveToForeignPtrWrapper
{-# DEPRECATED moveStorePath "Use 'moveToForeignPtrWrapper' instead" #-}

-- | Move ownership of a Ptr NixStorePath into 'StorePath'
moveStorePathMaybe :: Ptr NixStorePath -> IO (Maybe StorePath)
moveStorePathMaybe = nullableMoveToForeignPtrWrapper
{-# DEPRECATED moveStorePathMaybe "Use 'nullableMoveToForeignPtrWrapper' instead" #-}

instance Prelude.Show StorePath where
  show storePath = unsafePerformIO do
    bs <-
      BS.unsafePackMallocCString
        =<< [C.block| const char* {
          std::string s($fptr-ptr:(nix::StorePath *storePath)->to_string());
          return stringdup(s);
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
  moveToForeignPtrWrapper
    =<< [C.throwBlock| nix::StorePath *{
      return new StorePath(std::string($bs-ptr:bs, $bs-len:bs));
    }|]

-- | Parse a complete store path including storeDir into a 'StorePath'.
--
-- Throws C++ `BadStorePath` exception when invalid.
parseStorePath :: Store -> ByteString -> IO StorePath
parseStorePath (Store store) bs =
  moveToForeignPtrWrapper
    =<< [C.throwBlock| nix::StorePath *{
      return new StorePath(std::move((*$(refStore* store))->parseStorePath(std::string($bs-ptr:bs, $bs-len:bs))));
    }|]

getStorePathBaseName :: StorePath -> IO ByteString
getStorePathBaseName (StorePath sp) = do
  BS.unsafePackMallocCString
    =<< [C.block| const char *{
      std::string s($fptr-ptr:(nix::StorePath *sp)->to_string());
      return stringdup(s);
    }|]

getStorePathHash :: StorePath -> IO ByteString
getStorePathHash (StorePath sp) = do
  BS.unsafePackMallocCString
    =<< [C.block| const char *{
      std::string s($fptr-ptr:(nix::StorePath *sp)->hashPart());
      return stringdup(s);
    }|]

storePathToPath :: Store -> StorePath -> IO ByteString
storePathToPath (Store store) (StorePath sp) =
  BS.unsafePackMallocCString
    =<< [C.block| const char *{
      Store & store = **$(refStore* store);
      StorePath &sp = *$fptr-ptr:(nix::StorePath *sp);
      std::string s(store.printStorePath(sp));
      return stringdup(s);
    }|]

ensurePath :: Store -> StorePath -> IO ()
ensurePath (Store store) (StorePath storePath) =
  [C.throwBlock| void {
    ReceiveInterrupts _;
    Store &store = **$(refStore* store);
    StorePath &storePath = *$fptr-ptr:(nix::StorePath *storePath);
    store.ensurePath(storePath);
  } |]

addTemporaryRoot :: Store -> StorePath -> IO ()
addTemporaryRoot (Store store) storePath = do
  [C.throwBlock| void {
    ReceiveInterrupts _;
    Store &store = **$(refStore* store);
    StorePath &storePath = *$fptr-ptr:(nix::StorePath *storePath);
    store.addTempRoot(storePath);
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

instance HasEncapsulation C.NixStorePathWithOutputs StorePathWithOutputs

instance Finalizer C.NixStorePathWithOutputs where
  finalizer = finalizeStorePathWithOutputs -- must be CAF

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
{-# DEPRECATED finalizeStorePathWithOutputs "Use 'finalizer' instead" #-}

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
  [C.throwBlock| void {
    ReceiveInterrupts _;
    Store &store = **$(refStore* store);
    std::vector<StorePathWithOutputs> &paths = *$fptr-ptr:(std::vector<nix::StorePathWithOutputs>* paths);
    store.buildPaths(toDerivedPaths(paths));
  }|]

buildPath :: Store -> StorePathWithOutputs -> IO ()
buildPath store spwo = do
  buildPaths store =<< Std.Vector.fromListFP [spwo]

newtype Derivation = Derivation (ForeignPtr C.Derivation)

instance HasEncapsulation C.Derivation Derivation

instance Finalizer C.Derivation where
  finalizer = finalizeDerivation -- must be CAF

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
{-# DEPRECATED finalizeDerivation "Use 'finalizer' instead" #-}

getDerivation :: Store -> StorePath -> IO Derivation
getDerivation (Store store) (StorePath spwo) = do
  moveToForeignPtrWrapper
    =<< [C.throwBlock| Derivation *{
      ReceiveInterrupts _;
      Store &store = **$(refStore* store);
      return new Derivation(
          store.derivationFromPath(*$fptr-ptr:(nix::StorePath *spwo))
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
getDerivationFromString (Store store) name contents = do
  moveToForeignPtrWrapper
    =<< [C.throwBlock| Derivation *{
      Store &store = **$(refStore* store);
      std::string name($bs-ptr:name, $bs-len:name);
      return new Derivation(parseDerivation(store, std::string($bs-ptr:contents, $bs-len:contents), name));
    }|]

getDerivationNameFromPath :: StorePath -> IO ByteString
getDerivationNameFromPath storePath =
  BS.unsafePackMallocCString
    =<< [C.throwBlock| const char *{
      StorePath &sp = *$fptr-ptr:(nix::StorePath *storePath);
      std::string s(Derivation::nameFromPath(sp));
      return stringdup(s);
    }|]

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
#if NIX_IS_AT_LEAST(2, 24, 0)
getDerivationOutputs (Store store) drvName (Derivation derivationFPtr) =
  withForeignPtr derivationFPtr \derivation ->
  withDelete
    [C.exp| DerivationOutputsIterator* {
      new DerivationOutputsIterator($(Derivation *derivation)->outputs.begin())
    }|] \i ->
    fix $ \continue -> do
      isEnd <- (0 /=) <$> [C.exp| bool { *$(DerivationOutputsIterator *i) == $(Derivation *derivation)->outputs.end() }|]
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
                    name = stringdup(nameString);
                    path = nullptr;
                    std::visit(overloaded {
                      [&](DerivationOutput::InputAddressed doi) -> void {
                        typ = 0;
                        path = new StorePath(doi.path);
                      },
                      [&](DerivationOutput::CAFixed dof) -> void {
                        typ = 1;
                        path = new StorePath(dof.path(store, $(Derivation *derivation)->name, nameString));
                        switch (dof.ca.method.raw) {
                          case ContentAddressMethod::Raw::Text:
                            // FIXME (RFC 92)
                            fim = -1;
                            break;
                          case ContentAddressMethod::Raw::Flat:
                            fim = 0;
                            break;
                          case ContentAddressMethod::Raw::NixArchive:
                            fim = 1;
                            break;
                          case ContentAddressMethod::Raw::Git:
                            // FIXME (git-hashing)
                            fim = -1;
                            break;
                          default:
                            fim = -1;
                            break;
                        }

                        const Hash & hash = dof.ca.hash;

                        switch (hash.algo) {
                          case HashAlgorithm::MD5:
                            hashType = 0;
                            break;
                          case HashAlgorithm::SHA1:
                            hashType = 1;
                            break;
                          case HashAlgorithm::SHA256:
                            hashType = 2;
                            break;
                          case HashAlgorithm::SHA512:
                            hashType = 3;
                            break;
                          default:
                            hashType = -1;
                            break;
                        }
                        hashSize = hash.hashSize;
                        hashValue = (char*)malloc(hashSize);
                        std::memcpy((void*)(hashValue),
                                    (void*)(hash.hash),
                                    hashSize);
                      },
                      [&](DerivationOutput::CAFloating dof) -> void {
                        typ = 2;
                        switch(dof.method.raw) {
                          case ContentAddressMethod::Raw::Text:
                            // FIXME (RFC 92)
                            fim = -1;
                            break;
                          case ContentAddressMethod::Raw::Flat:
                            fim = 0;
                            break;
                          case ContentAddressMethod::Raw::NixArchive:
                            fim = 1;
                            break;
                          case ContentAddressMethod::Raw::Git:
                            // FIXME (git-hashing)
                            fim = -1;
                            break;
                          default:
                            fim = -1;
                            break;
                        }
                        switch (dof.hashAlgo) {
                          case HashAlgorithm::MD5:
                            hashType = 0;
                            break;
                          case HashAlgorithm::SHA1:
                            hashType = 1;
                            break;
                          case HashAlgorithm::SHA256:
                            hashType = 2;
                            break;
                          case HashAlgorithm::SHA512:
                            hashType = 3;
                            break;
                          default:
                            hashType = -1;
                            break;
                        }
                      },
                      [&](DerivationOutput::Deferred) -> void {
                        typ = 3;
                      },
                      [&](DerivationOutput::Impure) -> void {
                        typ = 4;
                      },
                    },
                    i->second.raw
                    );
                    i++;
                  }|]
                  name <- unsafePackMallocCString =<< peek nameP
                  path <- nullableMoveToForeignPtrWrapper =<< peek pathP
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
                    4 -> panic "getDerivationOutputs: impure derivations not supported yet"
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
#else
-- < 2.24
getDerivationOutputs (Store store) drvName (Derivation derivationFPtr) =
  withForeignPtr derivationFPtr \derivation ->
  withDelete
    [C.exp| DerivationOutputsIterator* {
      new DerivationOutputsIterator($(Derivation *derivation)->outputs.begin())
    }|] \i ->
    fix $ \continue -> do
      isEnd <- (0 /=) <$> [C.exp| bool { *$(DerivationOutputsIterator *i) == $(Derivation *derivation)->outputs.end() }|]
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
                    name = stringdup(nameString);
                    path = nullptr;
                    std::visit(overloaded {
#if NIX_IS_AT_LEAST(2, 18, 0)
                      [&](DerivationOutput::InputAddressed doi) -> void {
                        typ = 0;
                        path = new StorePath(doi.path);
                      },
                      [&](DerivationOutput::CAFixed dof) -> void {
                        typ = 1;
                        path = new StorePath(dof.path(store, $(Derivation *derivation)->name, nameString));
                        std::visit(overloaded {
                          [&](nix::FileIngestionMethod fim_) -> void {
                            switch (fim_) {
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
                          },
                          [&](nix::TextIngestionMethod) -> void {
                            // FIXME (RFC 92)
                            fim = -1;
                          }
                        }, dof.ca.method.raw);

                        const Hash & hash = dof.ca.hash;

#if NIX_IS_AT_LEAST(2, 20, 0)
                        switch (hash.algo) {
                          case HashAlgorithm::MD5:
                            hashType = 0;
                            break;
                          case HashAlgorithm::SHA1:
                            hashType = 1;
                            break;
                          case HashAlgorithm::SHA256:
                            hashType = 2;
                            break;
                          case HashAlgorithm::SHA512:
                            hashType = 3;
                            break;
#else
                        switch (hash.type) {
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
#endif
                          default:
                            hashType = -1;
                            break;
                        }
                        hashSize = hash.hashSize;
                        hashValue = (char*)malloc(hashSize);
                        std::memcpy((void*)(hashValue),
                                    (void*)(hash.hash),
                                    hashSize);
                      },
                      [&](DerivationOutput::CAFloating dof) -> void {
                        typ = 2;
                        std::visit(overloaded {
                          [&](nix::FileIngestionMethod fim_) -> void {
                            switch (fim_) {
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
                          },
                          [&](nix::TextIngestionMethod) -> void {
                            // FIXME (RFC 92)
                            fim = -1;
                          }
                        }, dof.method.raw);
#if NIX_IS_AT_LEAST(2, 20, 0)
                        switch (dof.hashAlgo) {
                          case HashAlgorithm::MD5:
                            hashType = 0;
                            break;
                          case HashAlgorithm::SHA1:
                            hashType = 1;
                            break;
                          case HashAlgorithm::SHA256:
                            hashType = 2;
                            break;
                          case HashAlgorithm::SHA512:
                            hashType = 3;
                            break;
#else
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
#endif
                          default:
                            hashType = -1;
                            break;
                        }
                      },
                      [&](DerivationOutput::Deferred) -> void {
                        typ = 3;
                      },
                      [&](DerivationOutput::Impure) -> void {
                        typ = 4;
                      },
                    },
                    i->second.raw
#else
                      [&](DerivationOutputInputAddressed doi) -> void {
                        typ = 0;
                        path = new StorePath(doi.path);
                      },
                      [&](DerivationOutputCAFixed dof) -> void {
                        typ = 1;
                        path = new StorePath(dof.path(store, $(Derivation *derivation)->name, nameString));
#if NIX_IS_AT_LEAST(2, 16, 0)
                        std::visit(overloaded {
                          [&](nix::FileIngestionMethod fim_) -> void {
                            switch (fim_) {
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
                          },
                          [&](nix::TextIngestionMethod) -> void {
                            // FIXME (RFC 92)
                            fim = -1;
                          }
#  if NIX_IS_AT_LEAST(2, 17, 0)
                        }, dof.ca.method.raw);
#  else
                        }, dof.ca.getMethod().raw);
#  endif
#else
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
#endif

#if NIX_IS_AT_LEAST(2, 17, 0)
                        const Hash & hash = dof.ca.hash;
#elif NIX_IS_AT_LEAST(2, 16, 0)
                        const Hash & hash = dof.ca.getHash();
#else
                        const Hash & hash = dof.hash.hash;
#endif
                        switch (hash.type) {
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
                        hashSize = hash.hashSize;
                        hashValue = (char*)malloc(hashSize);
                        std::memcpy((void*)(hashValue),
                                    (void*)(hash.hash),
                                    hashSize);
                      },
                      [&](DerivationOutputCAFloating dof) -> void {
                        typ = 2;
#if NIX_IS_AT_LEAST(2, 16, 0)
                        std::visit(overloaded {
                          [&](nix::FileIngestionMethod fim_) -> void {
                            switch (fim_) {
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
                          },
                          [&](nix::TextIngestionMethod) -> void {
                            // FIXME (RFC 92)
                            fim = -1;
                          }
                        }, dof.method.raw);
#else
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
#endif
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
#if NIX_IS_AT_LEAST(2,8,0)
                      [&](DerivationOutputImpure) -> void {
                        typ = 4;
                      },
#endif
                    },
#if NIX_IS_AT_LEAST(2,8,0)
                      i->second.raw()
#else
                      i->second.output
#endif
#endif
                    );
                    i++;
                  }|]
                  name <- unsafePackMallocCString =<< peek nameP
                  path <- nullableMoveToForeignPtrWrapper =<< peek pathP
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
                    4 -> panic "getDerivationOutputs: impure derivations not supported yet"
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
#endif

instance Delete DerivationOutputsIterator where
  delete a = [C.block| void { delete $(DerivationOutputsIterator *a); }|]

deleteDerivationOutputsIterator :: Ptr DerivationOutputsIterator -> IO ()
deleteDerivationOutputsIterator = delete
{-# DEPRECATED deleteDerivationOutputsIterator "Use 'delete' instead" #-}

getDerivationPlatform :: Derivation -> IO ByteString
getDerivationPlatform derivation =
  BS.unsafePackMallocCString =<<
    [C.exp| const char* {
       stringdup($fptr-ptr:(Derivation *derivation)->platform)
     } |]

getDerivationBuilder :: Derivation -> IO ByteString
getDerivationBuilder derivation =
  BS.unsafePackMallocCString =<<
    [C.exp| const char* {
       stringdup($fptr-ptr:(Derivation *derivation)->builder)
     } |]

getDerivationArguments :: Derivation -> IO [ByteString]
getDerivationArguments derivation =
  withDelete
    [C.throwBlock| Strings* {
      Strings *r = new Strings();
      for (auto i : $fptr-ptr:(Derivation *derivation)->args) {
        r->push_back(i);
      }
      return r;
    }|]
    toByteStrings

getDerivationSources :: Store -> Derivation -> IO [StorePath]
getDerivationSources _ = getDerivationSources'

getDerivationSources' :: Derivation -> IO [StorePath]
getDerivationSources' derivation = mask_ do
  vec <-
    moveToForeignPtrWrapper
      =<< [C.throwBlock| std::vector<nix::StorePath*>* {
        auto r = new std::vector<StorePath *>();
        for (auto s : $fptr-ptr:(Derivation *derivation)->inputSrcs)
          r->push_back(new StorePath(s));
        return r;
      }|]
  traverse moveToForeignPtrWrapper =<< Std.Vector.toList vec

getDerivationInputs :: Store -> Derivation -> IO [(StorePath, [ByteString])]
getDerivationInputs _ = getDerivationInputs'

-- | Get the inputs of a derivation, ignoring dependencies on outputs of outputs (RFC 92 inputs).
getDerivationInputs' :: Derivation -> IO [(StorePath, [ByteString])]
#if NIX_IS_AT_LEAST(2, 18, 0)
getDerivationInputs' (Derivation derivationFPtr) =
  withForeignPtr derivationFPtr \derivation ->
  withDelete
    [C.exp| DerivationInputsIterator* {
      new DerivationInputsIterator($(Derivation *derivation)->inputDrvs.map.begin())
    }|]
    $ \i -> fix $ \continue -> do
      isEnd <- (0 /=) <$> [C.exp| bool { *$(DerivationInputsIterator *i) == $(Derivation *derivation)->inputDrvs.map.end() }|]
      if isEnd
        then pure []
        else do
          name <-
            [C.throwBlock| nix::StorePath *{
              return new StorePath((*$(DerivationInputsIterator *i))->first);
            }|]
              >>= moveToForeignPtrWrapper
          outs <-
            withDelete
              [C.block| Strings* {
                Strings *r = new Strings();

                for (const auto & i : (*$(DerivationInputsIterator *i))->second.value) {
                  r->push_back(i);
                }

                // for (const auto &i : iter->second.childMap) {
                // TODO (RFC 92)
                //}

                return r;
              }|]
              toByteStrings
          [C.block| void { (*$(DerivationInputsIterator *i))++; }|]
          ((name, outs) :) <$> continue
#else
getDerivationInputs' (Derivation derivationFPtr) =
  withForeignPtr derivationFPtr \derivation ->
  withDelete
    [C.exp| DerivationInputsIterator* {
      new DerivationInputsIterator($(Derivation *derivation)->inputDrvs.begin())
    }|]
    $ \i -> fix $ \continue -> do
      isEnd <- (0 /=) <$> [C.exp| bool { *$(DerivationInputsIterator *i) == $(Derivation *derivation)->inputDrvs.end() }|]
      if isEnd
        then pure []
        else do
          name <-
            [C.throwBlock| nix::StorePath *{
              return new StorePath((*$(DerivationInputsIterator *i))->first);
            }|]
              >>= moveToForeignPtrWrapper
          outs <-
            withDelete
              [C.block| Strings*{ 
                Strings *r = new Strings();
                for (auto i : (*$(DerivationInputsIterator *i))->second) {
                  r->push_back(i);
                }
                return r;
              }|]
              toByteStrings
          [C.block| void { (*$(DerivationInputsIterator *i))++; }|]
          ((name, outs) :) <$> continue
#endif

instance Delete DerivationInputsIterator where
  delete a = [C.block| void { delete $(DerivationInputsIterator *a); }|]

deleteDerivationInputsIterator :: Ptr DerivationInputsIterator -> IO ()
deleteDerivationInputsIterator = delete
{-# DEPRECATED deleteDerivationInputsIterator "Use 'delete' instead" #-}

getDerivationEnv :: Derivation -> IO (Map ByteString ByteString)
getDerivationEnv (Derivation fptr) =
  withForeignPtr fptr \ptr -> do
    pairs <- [C.exp| StringPairs* { &$(Derivation *ptr)->env }|]
    toByteStringMap pairs

getDerivationOutputNames :: ForeignPtr C.Derivation -> IO [ByteString]
getDerivationOutputNames fptr =
  withForeignPtr fptr \ptr -> withDelete
    [C.throwBlock| Strings* {
      Strings *r = new Strings();
      for (auto i : $(Derivation *ptr)->outputs) {
        r->push_back(i.first);
      }
      return r;
    }|]
    toByteStrings

instance Delete StringPairs where
  delete s = [C.block| void { delete $(StringPairs *s); }|]

deleteStringPairs :: Ptr StringPairs -> IO ()
deleteStringPairs = delete
{-# DEPRECATED deleteStringPairs "Use 'delete' instead" #-}

instance Delete Strings where
  delete s = [C.block| void { delete $(Strings *s); }|]

deleteStrings :: Ptr Strings -> IO ()
deleteStrings = delete
{-# DEPRECATED deleteStrings "Use 'delete' instead" #-}

instance Finalizer Strings where
  finalizer = finalizeStrings -- must be CAF

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
{-# DEPRECATED finalizeStrings "Use 'finalizer' instead" #-}

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
        s <- [C.exp| const char*{ stringdup(*(*$(StringsIterator *i))) }|]
        bs <- BS.unsafePackMallocCString s
        [C.block| void { (*$(StringsIterator *i))++; }|]
        (bs :) <$> go

toByteStringMap :: Ptr StringPairs -> IO (Map ByteString ByteString)
toByteStringMap strings =
  M.fromList <$> withStringPairIterator \i ->
    fix $ \go -> do
      isEnd <- (0 /=) <$> [C.exp| bool { *$(StringPairsIterator *i) == $(StringPairs *strings)->end() }|]
      if isEnd
        then pure []
        else do
          k <- [C.exp| const char*{ stringdup((*$(StringPairsIterator *i))->first) }|]
          v <- [C.exp| const char*{ stringdup((*$(StringPairsIterator *i))->second) }|]
          bk <- BS.unsafePackMallocCString k
          bv <- BS.unsafePackMallocCString v
          [C.block| void { (*$(StringPairsIterator *i))++; }|]
          ((bk, bv) :) <$> go
  where
    withStringPairIterator =
      withDelete
        [C.exp| StringPairsIterator *{ new StringPairsIterator($(StringPairs *strings)->begin()) }|]

instance Delete C.StringPairsIterator where
  delete i = [C.block| void { delete $(StringPairsIterator *i); }|]

withStrings :: (Ptr Strings -> IO a) -> IO a
withStrings =
  withDelete
    [C.exp| Strings *{ new Strings() }|]

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
      ReceiveInterrupts _;
      ref<Store> src = *$(refStore* src);
      ref<Store> dest = *$(refStore* dest);
      std::vector<nix::StorePath *> &pathsVector = *$(std::vector<nix::StorePath*>* pathsVector);

      StorePathSet pathSet;
      for (auto spp : pathsVector)
        pathSet.insert(*spp);

      StorePathSet closurePaths;
      src->computeFSClosure(pathSet, closurePaths);

      nix::copyPaths(*src, *dest, closurePaths);
    }|]
  for_ pathList (\(StorePath c) -> touchForeignPtr c)

parseSecretKey :: ByteString -> IO (ForeignPtr SecretKey)
parseSecretKey bs =
  [C.throwBlock| SecretKey* {
    return new SecretKey($bs-cstr:bs);
  }|]
    >>= toForeignPtr

instance Finalizer SecretKey where
  finalizer = finalizeSecretKey -- must be CAF

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
{-# DEPRECATED finalizeSecretKey "Use 'finalizer' instead" #-}

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
    ReceiveInterrupts _;
    nix::ref<nix::Store> store = *$(refStore *store);
    const StorePath &storePath = *$fptr-ptr:(nix::StorePath *path);
    const SecretKey &secretKey = *$(SecretKey *secretKey);
    auto currentInfo = store->queryPathInfo(storePath);

    auto info2(*currentInfo);
    info2.sigs.clear();
#if NIX_IS_AT_LEAST(2, 20, 0)
    {
      auto signer = std::make_unique<LocalSigner>(SecretKey { secretKey });
      info2.sign(*store, *signer);
    }
#else
    info2.sign(*store, secretKey);
#endif
    assert(!info2.sigs.empty());
    auto sig = *info2.sigs.begin();

    if (currentInfo->sigs.count(sig)) {
      return 0;
    } else {
      store->addSignatures(storePath, info2.sigs);
      return 1;
    }
  }|]

-- | Follow symlinks to the store and chop off the parts after the top-level store name
followLinksToStorePath :: Store -> ByteString -> IO StorePath
followLinksToStorePath (Store store) bs =
  moveToForeignPtrWrapper
    =<< [C.throwBlock| nix::StorePath *{
      ReceiveInterrupts _;
      Store &store = **$(refStore* store);
      std::string s = std::string($bs-ptr:bs, $bs-len:bs);
      return new StorePath(store.followLinksToStorePath(s));
    }|]

-- | Whether a path exists and is registered.
isValidPath :: Store -> StorePath -> IO Bool
isValidPath (Store store) path =
  [C.throwBlock| bool {
    ReceiveInterrupts _;
    Store &store = **$(refStore* store);
    StorePath &path = *$fptr-ptr:(nix::StorePath *path);
    return store.isValidPath(path);
  }|]
    <&> (/= 0)

queryPathInfo ::
  Store ->
  -- | Exact store path, not a subpath
  StorePath ->
  -- | ValidPathInfo or exception
  IO (ForeignPtr (Ref ValidPathInfo))
queryPathInfo (Store store) (StorePath pathFPtr) = 
  withForeignPtr pathFPtr \path -> do
  vpi <-
    [C.throwBlock| refValidPathInfo* {
      ReceiveInterrupts _;
      Store &store = **$(refStore* store);
      StorePath &path = *$(nix::StorePath *path);
      return new refValidPathInfo(store.queryPathInfo(path));
    }|]
  toForeignPtr vpi

-- | Query only the local client cache ("narinfo cache") - does not query the actual store or daemon.
--
-- Returns 'Nothing' if nothing is known about the path.
-- Returns 'Just Nothing' if the path is known to not exist.
-- Returns 'Just (Just vpi)' if the path is known to exist, with the given 'ValidPathInfo'.
queryPathInfoFromClientCache ::
  Store ->
  StorePath ->
  IO (Maybe (Maybe (ForeignPtr (Ref ValidPathInfo))))
queryPathInfoFromClientCache (Store store) (StorePath pathFPtr) =
  withForeignPtr pathFPtr \path ->
#if NIX_IS_AT_LEAST(2, 20, 0)
  alloca \isKnownP -> do
    mvpi <- [C.throwBlock| refValidPathInfo* {
        ReceiveInterrupts _;
        Store &store = **$(refStore* store);
        StorePath &path = *$(nix::StorePath *path);
        bool &isKnown = *$(bool* isKnownP);
        std::optional<std::shared_ptr<const ValidPathInfo>> maybeVPI =
            store.queryPathInfoFromClientCache(path);
        if (!maybeVPI) {
          isKnown = false;
          return nullptr;
        }
        else {
          isKnown = true;
          std::shared_ptr<const ValidPathInfo> &vpi = *maybeVPI;
          if (vpi)
            return new refValidPathInfo(vpi);
          else
            return nullptr;
        }
      }|]
    isKnown <- peek isKnownP <&> (/= 0)
    for (guard isKnown) \_ -> do
      mvpi & traverseNonNull toForeignPtr
#else
  alloca \isKnownP -> do
    mvpi <- [C.throwBlock| refValidPathInfo* {
        ReceiveInterrupts _;
        Store &store = **$(refStore* store);
        StorePath &path = *$(nix::StorePath *path);
        bool &isKnown = *$(bool* isKnownP);

        std::string uri = store.getUri();
        ref<NarInfoDiskCache> cache = nix::getNarInfoDiskCache();

        // std::pair<nix::NarInfoDiskCache::Outcome, std::shared_ptr<NarInfo>>
        auto [outcome, maybeNarInfo] =
          cache->lookupNarInfo(uri, std::string(path.hashPart()));

        if (outcome == nix::NarInfoDiskCache::oValid) {
          assert(maybeNarInfo);
          isKnown = true;
          return new refValidPathInfo(maybeNarInfo);
        }
        else if (outcome == nix::NarInfoDiskCache::oInvalid) {
          isKnown = true;
          return nullptr;
        }
        else {
          // nix::NarInfoDiskCache::oUnknown or unexpected value
          isKnown = false;
          return nullptr;
        }
      }|]
    isKnown <- peek isKnownP <&> (/= 0)
    for (guard isKnown) \_ -> do
      mvpi & traverseNonNull toForeignPtr
#endif

instance Finalizer (Ref ValidPathInfo) where
  finalizer = finalizeRefValidPathInfo -- must be CAF

finalizeRefValidPathInfo :: FinalizerPtr (Ref ValidPathInfo)
{-# NOINLINE finalizeRefValidPathInfo #-}
finalizeRefValidPathInfo =
  unsafePerformIO
    [C.exp|
      void (*)(refValidPathInfo *) {
        [](refValidPathInfo *v){ delete v; }
      }|]
{-# DEPRECATED finalizeRefValidPathInfo "Use 'finalizer' instead" #-}

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
#if NIX_IS_AT_LEAST(2,20,0)
      std::string s((*$fptr-ptr:(refValidPathInfo* vpi))->narHash.to_string(nix::HashFormat::Nix32, true));
#elif NIX_IS_AT_LEAST(2,19,0)
      std::string s((*$fptr-ptr:(refValidPathInfo* vpi))->narHash.to_string(nix::HashFormat::Base32, true));
#else
      std::string s((*$fptr-ptr:(refValidPathInfo* vpi))->narHash.to_string(nix::Base32, true));
#endif
      return stringdup(s);
    }|]

-- | Deriver field of a ValidPathInfo struct. Source: store-api.hh
validPathInfoDeriver :: Store -> ForeignPtr (Ref ValidPathInfo) -> IO (Maybe StorePath)
validPathInfoDeriver _ = validPathInfoDeriver'

validPathInfoDeriver' :: ForeignPtr (Ref ValidPathInfo) -> IO (Maybe StorePath)
validPathInfoDeriver' vpi =
  nullableMoveToForeignPtrWrapper
    =<< [C.throwBlock| nix::StorePath * {
      std::optional<StorePath> deriver = (*$fptr-ptr:(refValidPathInfo* vpi))->deriver;
      return deriver ? new StorePath(*deriver) : nullptr;
    }|]

-- | References field of a ValidPathInfo struct. Source: store-api.hh
validPathInfoReferences :: Store -> ForeignPtr (Ref ValidPathInfo) -> IO [StorePath]
validPathInfoReferences _ = validPathInfoReferences'

validPathInfoReferences' :: ForeignPtr (Ref ValidPathInfo) -> IO [StorePath]
validPathInfoReferences' vpi = do
  sps <-
    moveToForeignPtrWrapper
      =<< [C.throwBlock| std::vector<nix::StorePath *>* {
        auto sps = new std::vector<nix::StorePath *>();
        for (auto sp : (*$fptr-ptr:(refValidPathInfo* vpi))->references)
          sps->push_back(new StorePath(sp));
        return sps;
      }|]
  l <- Std.Vector.toList sps
  for l moveToForeignPtrWrapper

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
    ReceiveInterrupts _;
    Store &store = **$(refStore* store);
    StorePathSet &ret = *$fptr-ptr:(std::set<nix::StorePath>* retSet);
    store.computeFSClosure(*$fptr-ptr:(std::set<nix::StorePath>* startingSet), ret,
      $(int flipDir), $(int inclOut), $(int inclDrv));
  }|]
  pure ret

withPtr' :: (Coercible a' (ForeignPtr a)) => a' -> (Ptr a -> IO b) -> IO b
withPtr' p = withForeignPtr (coerce p)
{-# DEPRECATED withPtr' "Use 'HasEncapsulation' instead" #-}
