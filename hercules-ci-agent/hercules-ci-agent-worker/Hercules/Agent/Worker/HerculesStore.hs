{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
#ifdef __GHCIDE__
# define NIX_IS_AT_LEAST(mm,m,p) 1
#endif

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
import Hercules.CNix.Expr.Context (EvalState, NixStorePathWithOutputs)
import Hercules.CNix.Std.Vector (CStdVector, StdVector)
import Hercules.CNix.Store.Context (Ref)
import Language.C.Inline.Cpp qualified as C
import Language.C.Inline.Cpp.Exception qualified as C
import Protolude
import Prelude ()

C.context context

C.include "<cstring>"

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
          refHerculesStore hs(new HerculesStore(s));
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

{- ORMOLU_DISABLE -}

-- TODO catch pure exceptions from displayException

-- | Set the callback that is invoked when the evaluator requests a build.
--
-- The 'EvalState' is referenced by the exception the callback throws into
-- the evaluator: on Nix >= 2.34 the exception must be recoverable, so that
-- the interrupted attribute can be re-evaluated after its realisation
-- completes. It must remain valid for as long as the callback can be
-- invoked, i.e. for the duration of the evaluation.
setBuilderCallback :: Ptr (Ref HerculesStore) -> Ptr EvalState -> (StdVector NixStorePathWithOutputs -> IO ()) -> IO ()
setBuilderCallback s evalState callback = do
  p <-
    mkBuilderCallback $ \sp exceptionToThrowPtr ->
      Control.Exception.catch (callback =<< moveToForeignPtrWrapper sp) $ \e ->
        withCString (displayException (e :: SomeException)) $ \renderedException -> do
          stablePtr <- castStablePtrToPtr <$> newStablePtr e
          [C.block| void {
#if NIX_IS_AT_LEAST(2, 34, 0)
            nix::EvalState *evalState = $(EvalState *evalState);
            assert(evalState);
            (*$(exception_ptr *exceptionToThrowPtr)) = std::make_exception_ptr(HerculesBuilderException(*evalState, std::string($(const char* renderedException)), $(void* stablePtr)));
#else
            (void) $(EvalState *evalState);
            (*$(exception_ptr *exceptionToThrowPtr)) = std::make_exception_ptr(HaskellException(std::string($(const char* renderedException)), $(void* stablePtr)));
#endif
          }|]
  [C.throwBlock| void {
    (*$(refHerculesStore* s))->setBuilderCallback($(void (*p)(std::vector<nix::StorePathWithOutputs>*, exception_ptr *)));
  }|]
{- ORMOLU_ENABLE -}

type BuilderCallback = Ptr (CStdVector NixStorePathWithOutputs) -> Ptr ExceptionPtr -> IO ()

-- Work around a problem in ghcide with foreign imports.
#ifndef __GHCIDE__
foreign import ccall "wrapper"
  mkBuilderCallback :: BuilderCallback -> IO (FunPtr BuilderCallback)
#else
mkBuilderCallback :: BuilderCallback -> IO (FunPtr BuilderCallback)
mkBuilderCallback = panic "This is a stub to work around a ghcide issue. Please compile without -D__GHCIDE__"
#endif
