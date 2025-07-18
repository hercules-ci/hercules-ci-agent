{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.Agent.Worker.HerculesStore
  ( withHerculesStore,
    HerculesStore,
    nixStore,
    printDiagnostics,
    setBuilderCallback,
  )
where

import Control.Exception
  ( catch,
  )
import Data.Coerce (coerce)
import Foreign.C.String (withCString)
import Foreign.StablePtr (castStablePtrToPtr, newStablePtr)
import Hercules.Agent.Worker.HerculesStore.Context
  ( ExceptionPtr,
    HerculesStore,
    context,
  )
import Hercules.CNix.Encapsulation (moveToForeignPtrWrapper)
import Hercules.CNix.Expr (Store (Store))
import Hercules.CNix.Expr.Context (NixStorePathWithOutputs)
import Hercules.CNix.Std.Vector (CStdVector, StdVector)
import Hercules.CNix.Store.Context (Ref)
import Language.C.Inline.Cpp qualified as C
import Language.C.Inline.Cpp.Exception qualified as C
import Protolude
import Prelude ()

C.context context

C.include "<cstring>"

C.include "<nix/main/shared.hh>"

C.include "hercules-aliases.h"

C.using "namespace nix"

withHerculesStore ::
  Store ->
  (Ptr (Ref HerculesStore) -> IO a) ->
  IO a
withHerculesStore (Store wrappedStore) =
  bracket
    ( liftIO
        [C.block| refHerculesStore* {
          refStore &s = *$(refStore *wrappedStore);
          refHerculesStore hs(new HerculesStore({}, s));
          return new refHerculesStore(hs);
        } |]
    )
    (\x -> liftIO [C.exp| void { delete $(refHerculesStore* x) } |])

nixStore :: Ptr (Ref HerculesStore) -> Store
nixStore = coerce

printDiagnostics :: Ptr (Ref HerculesStore) -> IO ()
printDiagnostics s =
  [C.throwBlock| void{
    (*$(refHerculesStore* s))->printDiagnostics();
  }|]

-- TODO catch pure exceptions from displayException
setBuilderCallback :: Ptr (Ref HerculesStore) -> (StdVector NixStorePathWithOutputs -> IO ()) -> IO ()
setBuilderCallback s callback = do
  p <-
    mkBuilderCallback $ \sp exceptionToThrowPtr ->
      Control.Exception.catch (callback =<< moveToForeignPtrWrapper sp) $ \e ->
        withCString (displayException (e :: SomeException)) $ \renderedException -> do
          stablePtr <- castStablePtrToPtr <$> newStablePtr e
          [C.block| void {
            (*$(exception_ptr *exceptionToThrowPtr)) = std::make_exception_ptr(HaskellException(std::string($(const char* renderedException)), $(void* stablePtr)));
          }|]
  [C.throwBlock| void {
    (*$(refHerculesStore* s))->setBuilderCallback($(void (*p)(std::vector<nix::StorePathWithOutputs>*, exception_ptr *)));
  }|]

type BuilderCallback = Ptr (CStdVector NixStorePathWithOutputs) -> Ptr ExceptionPtr -> IO ()

-- Work around a problem in ghcide with foreign imports.
#ifndef __GHCIDE__
foreign import ccall "wrapper"
  mkBuilderCallback :: BuilderCallback -> IO (FunPtr BuilderCallback)
#else
mkBuilderCallback :: BuilderCallback -> IO (FunPtr BuilderCallback)
mkBuilderCallback = panic "This is a stub to work around a ghcide issue. Please compile without -D__GHCIDE__"
#endif
