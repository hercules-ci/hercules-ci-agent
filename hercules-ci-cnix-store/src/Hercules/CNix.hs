{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.CNix
  ( module Hercules.CNix,
    module Hercules.CNix.Store,
    type NixStore,
    type Ref,
    type SecretKey,
  )
where

-- TODO: No more Ptr EvalState
-- TODO: No more NixStore when EvalState is already there
-- TODO: Map Nix-specific C++ exceptions to a CNix exception type

import Hercules.CNix.Store
import Hercules.CNix.Store.Context
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import Protolude hiding (evalState, throwIO)

C.context context

C.include "<stdio.h>"

C.include "<cstring>"

C.include "<math.h>"

C.include "<nix/config.h>"

C.include "<nix/shared.hh>"

C.include "<nix/store-api.hh>"

C.include "<nix/get-drvs.hh>"

C.include "<nix/derivations.hh>"

C.include "<nix/affinity.hh>"

C.include "<nix/globals.hh>"

C.include "hercules-ci-cnix/store.hxx"

C.include "<gc/gc.h>"

C.include "<gc/gc_cpp.h>"

C.include "<gc/gc_allocator.h>"

C.using "namespace nix"

init :: IO ()
init =
  void
    [C.throwBlock| void {
      nix::initNix();
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

appendString :: Ptr Strings -> ByteString -> IO ()
appendString ss s =
  [C.block| void {
    $(Strings *ss)->push_back(std::string($bs-ptr:s, $bs-len:s));
  }|]
