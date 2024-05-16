{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
-- 'Delete' and 'Finalizer' instances are necessarily orphan instances due to TH staging restrictions.
{-# OPTIONS_GHC -Wno-orphans #-}

module Hercules.CNix.Std.String
  ( -- * Context
    CStdString,
    stdStringCtx,

    -- * Functions
    moveToByteString,
    withString,

    -- * Low level functions
    new,
    delete,

    -- * Wrapper-based functions
    copyToByteString,
  )
where

import Control.Exception (mask_)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackMallocCStringLen)
import Foreign hiding (new)
import Hercules.CNix.Encapsulation
import Hercules.CNix.Memory (Delete (..), Finalizer (finalizer), withDelete)
import Hercules.CNix.Std.String.Context
import Hercules.CNix.Std.String.Instances ()
import qualified Language.C.Inline as C
import System.IO.Unsafe (unsafePerformIO)
import Prelude

C.context (stdStringCtx <> C.bsCtx <> C.fptrCtx)

C.include "<string>"
C.include "<cstring>"

moveToByteString :: Ptr CStdString -> IO ByteString
moveToByteString s = mask_ $ alloca \ptr -> alloca \sz -> do
  [C.block| void {
    const std::string &s = *$(std::string *s);
    size_t sz = *$(size_t *sz) = s.size();
    char *ptr = *$(char **ptr) = (char*)malloc(sz);
    std::memcpy((void *)ptr, s.c_str(), sz);
  }|]
  sz' <- peek sz
  ptr' <- peek ptr
  unsafePackMallocCStringLen (ptr', fromIntegral sz')

new :: ByteString -> IO (Ptr CStdString)
new bs =
  [C.block| std::string* {
    return new std::string($bs-ptr:bs, $bs-len:bs);
  }|]

instance Delete CStdString where
  delete bs = [C.block| void { delete $(std::string *bs); }|]

withString :: ByteString -> (Ptr CStdString -> IO a) -> IO a
withString bs = withDelete (new bs)

instance Finalizer CStdString where
  finalizer = finalize -- must be a CAF

finalize :: FinalizerPtr CStdString
{-# NOINLINE finalize #-}
finalize =
  unsafePerformIO
    [C.exp|
      void (*)(std::string *) {
        [](std::string *v) {
          delete v;
        }
      }
    |]

newtype StdString = StdString (ForeignPtr CStdString)

instance HasEncapsulation CStdString StdString

copyToByteString :: StdString -> IO ByteString
copyToByteString (StdString s) = mask_ $ alloca \ptr -> alloca \sz -> do
  [C.block| void {
    const std::string &s = *$fptr-ptr:(std::string *s);
    size_t sz = *$(size_t *sz) = s.size();
    char *ptr = *$(char **ptr) = (char*)malloc(sz);
    std::memcpy((void *)ptr, s.c_str(), sz);
  }|]
  sz' <- peek sz
  ptr' <- peek ptr
  unsafePackMallocCStringLen (ptr', fromIntegral sz')
