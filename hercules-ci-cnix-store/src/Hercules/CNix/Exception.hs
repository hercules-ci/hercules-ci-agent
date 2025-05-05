{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.CNix.Exception
  ( handleExceptions,
    handleExceptions',
    handleExceptionPtr,
  )
where

import Hercules.CNix.Store.Context (context)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Protolude
import qualified System.Environment

C.context context

#if NIX_IS_AT_LEAST(2, 28, 0)
C.include "<nix/main/shared.hh>"
C.include "<nix/store/globals.hh>"
#else
C.include "<nix/config.h>"
C.include "<nix/shared.hh>"
C.include "<nix/globals.hh>"
#endif

-- | Log C++ exceptions and call 'exitWith' the way Nix would exit when an
-- exception occurs.
handleExceptions :: IO a -> IO a
handleExceptions io = do
  progName <- System.Environment.getProgName
  handleExceptions' exitWith (toS progName) io

-- | Log C++ exceptions and call 'exitWith' the way Nix would exit.
handleExceptions' ::
  -- | What to do when Nix would want to exit with 'ExitCode'
  (ExitCode -> IO a) ->
  -- | Program name (command name)
  Text ->
  IO a ->
  IO a
handleExceptions' handleExit programName io =
  let select (C.CppStdException eptr _msg _t) = Just eptr
      select _ = Nothing

      convertExit 0 = ExitSuccess
      convertExit e = ExitFailure (fromIntegral e)

      doHandle = handleExit . convertExit <=< handleExceptionPtr (encodeUtf8 programName)
   in handleJust select doHandle io

-- | Low-level wrapper around @nix::handleExceptions(rethrow_exception(e))@.
handleExceptionPtr :: ByteString -> C.CppExceptionPtr -> IO C.CInt
handleExceptionPtr programName eptr =
  [C.throwBlock| int {
    auto & eptr = *$fptr-ptr:(std::exception_ptr *eptr);
    std::string programName($bs-ptr:programName, $bs-len:programName);
    return nix::handleExceptions(programName, [&]() {
      std::rethrow_exception(eptr);
    });
  }|]
