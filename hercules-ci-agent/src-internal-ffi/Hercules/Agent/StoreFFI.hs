{-# OPTIONS_GHC -fobject-code #-}
{-# LANGUAGE CPP #-}

module Hercules.Agent.StoreFFI where

import qualified Foreign.C
import Protolude

type BuilderCallback = Foreign.C.CString -> Ptr ExceptionPtr -> IO ()

-- Work around a problem in ghcide with foreign imports.
#ifndef GHCIDE
foreign import ccall "wrapper"
  mkBuilderCallback :: BuilderCallback -> IO (FunPtr BuilderCallback)
#else
mkBuilderCallback :: BuilderCallback -> IO (FunPtr BuilderCallback)
mkBuilderCallback = panic "This is a stub to work around a ghcide issue. Please compile without -DGHCIDE"
#endif

data ExceptionPtr
