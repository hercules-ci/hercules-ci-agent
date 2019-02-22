{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module CNix.Internal.Store where

import           Prelude()
import           Protolude

import qualified Language.C.Inline.Cpp as C
import           CNix.Internal.Context
import           Conduit

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


withStore
  :: MonadResource m
  => (Ptr (Ref NixStore) -> ConduitT i o m r)
  -> ConduitT i o m r
withStore = bracketP (liftIO $ [C.block| refStore* {
      refStore s = openStore();
      return new refStore(s);
    } |])
    (\x -> liftIO $ [C.exp| void { delete $(refStore* x) } |])

storeUri :: MonadIO m => Ptr (Ref NixStore) -> m ByteString
storeUri store = unsafeMallocBS [C.block| const char* {
             std::string uri = (*$(refStore* store))->getUri();
             return strdup(uri.c_str());
           } |]
