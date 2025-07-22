{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.CNix
  ( init,
    setTalkative,
    setDebug,
    setGlobalOption,
    setOption,
    logInfo,
    appendString,
    nixVersion,

    -- * Re-exports
    module Hercules.CNix.Store,
  )
where

-- TODO: No more Ptr EvalState
-- TODO: No more NixStore when EvalState is already there

import Data.ByteString.Unsafe (unsafePackMallocCString)
import Hercules.CNix.Store
import qualified Hercules.CNix.Util
import Hercules.CNix.Verbosity
  ( Verbosity (Debug, Talkative),
    setVerbosity,
  )
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Protolude hiding (evalState, throwIO)
import System.IO.Unsafe (unsafePerformIO)

C.context context

C.include "<nix/util/config-global.hh>"
C.include "<nix/store/derivations.hh>"
C.include "<nix/store/globals.hh>"

C.include "<gc/gc.h>"
C.include "<gc/gc_cpp.h>"
C.include "<gc/gc_allocator.h>"

C.include "hercules-ci-cnix/string.hxx"
C.include "hercules-ci-cnix/store.hxx"

C.using "namespace nix"
C.using "namespace hercules_ci_cnix"

-- | Initialize the Nix store library. If you also use the Nix evaluator, you should
-- call 'Hercules.CNix.Expr.init' instead.
init :: IO ()
init = do
  void
    [C.throwBlock| void {
      nix::initLibStore();
    } |]
  -- Install signal handlers using Haskell primitives (replaces nix::unix::startSignalHandlerThread)
  Hercules.CNix.Util.installDefaultSigINTHandler

setTalkative :: IO ()
setTalkative = setVerbosity Talkative

setDebug :: IO ()
setDebug = setVerbosity Debug

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

nixVersion :: ByteString
nixVersion = unsafePerformIO $ do
  p <-
    [C.exp| const char* {
      stringdup(nix::nixVersion)
    }|]
  unsafePackMallocCString p
{-# NOINLINE nixVersion #-}
