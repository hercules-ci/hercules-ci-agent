{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module CNix.Internal.Store
  ( module CNix.Internal.Store,
    HerculesStore,
  )
where

import CNix.Internal.Context
import Control.Exception
import qualified Data.ByteString.Unsafe as BS
import Data.Coerce
import Foreign.C.String (withCString)
import Foreign.ForeignPtr
import Foreign.StablePtr
import Hercules.Agent.StoreFFI
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

C.include "aliases.h"

C.using "namespace nix"

withStore ::
  (Ptr (Ref NixStore) -> IO r) ->
  IO r
withStore =
  bracket
    ( liftIO $
        [C.block| refStore* {
            refStore s = openStore();
            return new refStore(s);
          } |]
    )
    (\x -> liftIO $ [C.exp| void { delete $(refStore* x) } |])

storeUri :: MonadIO m => Ptr (Ref NixStore) -> m ByteString
storeUri store =
  unsafeMallocBS
    [C.block| const char* {
       std::string uri = (*$(refStore* store))->getUri();
       return strdup(uri.c_str());
     } |]

ensurePath :: Ptr (Ref NixStore) -> ByteString -> IO ()
ensurePath store path =
  [C.throwBlock| void {
    (*$(refStore* store))->ensurePath(std::string($bs-ptr:path, $bs-len:path));
  } |]

clearPathInfoCache :: Ptr (Ref NixStore) -> IO ()
clearPathInfoCache store =
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

buildPath :: Ptr (Ref NixStore) -> ByteString -> IO ()
buildPath store path =
  [C.throwBlock| void {
    PathSet ps({std::string($bs-ptr:path, $bs-len:path)});
    (*$(refStore* store))->buildPaths(ps);
   } |]

getDerivation :: Ptr (Ref NixStore) -> ByteString -> IO (ForeignPtr Derivation)
getDerivation store path = do
  ptr <-
    [C.throwBlock| Derivation *{
      return new Derivation(
          (*$(refStore* store))->derivationFromPath(std::string($bs-ptr:path, $bs-len:path))
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
derivationOutputPath :: ForeignPtr Derivation -> ByteString -> IO ByteString
derivationOutputPath fd outputName = withForeignPtr fd $ \d ->
  [C.throwBlock|
    const char *{
      std::string outputName($bs-ptr:outputName, $bs-len:outputName);
      Derivation *d = $(Derivation *d);
      return strdup(d->outputs.at(outputName).path.c_str());
    }
  |]
    >>= BS.unsafePackMallocCString

----- Hercules -----
withHerculesStore ::
  Ptr (Ref NixStore) ->
  (Ptr (Ref HerculesStore) -> IO a) ->
  IO a
withHerculesStore wrappedStore =
  bracket
    ( liftIO $
        [C.block| refHerculesStore* {
          refStore &s = *$(refStore *wrappedStore);
          refHerculesStore hs(new HerculesStore({}, s));
          return new refHerculesStore(hs);
        } |]
    )
    (\x -> liftIO $ [C.exp| void { delete $(refHerculesStore* x) } |])

nixStore :: Ptr (Ref HerculesStore) -> Ptr (Ref NixStore)
nixStore = coerce

printDiagnostics :: Ptr (Ref HerculesStore) -> IO ()
printDiagnostics s =
  [C.throwBlock| void{
    (*$(refHerculesStore* s))->printDiagnostics();
  }|]

-- TODO catch pure exceptions from displayException
setBuilderCallback :: Ptr (Ref HerculesStore) -> (ByteString -> IO ()) -> IO ()
setBuilderCallback s callback = do
  p <-
    mkBuilderCallback $ \cstr exceptionToThrowPtr ->
      Control.Exception.catch (BS.unsafePackMallocCString cstr >>= callback) $ \e ->
        withCString (displayException (e :: SomeException)) $ \renderedException -> do
          stablePtr <- castStablePtrToPtr <$> newStablePtr e
          [C.block| void {
            (*$(exception_ptr *exceptionToThrowPtr)) = std::make_exception_ptr(HaskellException(std::string($(const char* renderedException)), $(void* stablePtr)));
          }|]
  [C.throwBlock| void {
    (*$(refHerculesStore* s))->setBuilderCallback($(void (*p)(const char *, exception_ptr *) ));
  }|]
