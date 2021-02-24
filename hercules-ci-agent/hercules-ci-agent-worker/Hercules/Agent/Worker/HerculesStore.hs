{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.Agent.Worker.HerculesStore
  ( module Hercules.Agent.Worker.HerculesStore,
    HerculesStore,
  )
where

import Control.Exception
  ( catch,
  )
import qualified Data.ByteString.Unsafe as BS
import Data.Coerce (coerce)
import Foreign.C.String (withCString)
import Foreign.StablePtr (castStablePtrToPtr, newStablePtr)
import Hercules.Agent.StoreFFI (mkBuilderCallback)
import Hercules.Agent.Worker.HerculesStore.Context
  ( HerculesStore,
    context,
  )
import Hercules.CNix.Store.Context (NixStore, Ref)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import Protolude
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

C.include "hercules-aliases.h"

C.using "namespace nix"

withHerculesStore ::
  Ptr (Ref NixStore) ->
  (Ptr (Ref HerculesStore) -> IO a) ->
  IO a
withHerculesStore wrappedStore =
  bracket
    ( liftIO
        [C.block| refHerculesStore* {
          refStore &s = *$(refStore *wrappedStore);
          refHerculesStore hs(new HerculesStore({}, s));
          return new refHerculesStore(hs);
        } |]
    )
    (\x -> liftIO [C.exp| void { delete $(refHerculesStore* x) } |])

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
