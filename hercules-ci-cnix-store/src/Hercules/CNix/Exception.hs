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

C.include "<nix/store/globals.hh>"
C.include "<nix/util/logging.hh>"
C.include "<nix/util/signals.hh>"
C.include "<nix/util/error.hh>"

C.using "namespace nix"

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
    // Based on nix::handleExceptions, but without the libmain-specific stuff
    std::string error = ANSI_RED "error:" ANSI_NORMAL " ";
    try {
      try {
        try {
          std::rethrow_exception(eptr);
        } catch (...) {
          // Avoid throwing another interrupt error in the print routines that actually catch this.
          setInterruptThrown();
          throw;
        }
      } catch (BaseError & e) {
          logError(e.info());
          return e.info().status;
      } catch (std::bad_alloc & e) {
          printError(error + "out of memory");
          return 1;
      } catch (std::exception & e) {
          printError(error + e.what());
          return 1;
      }
    } catch (...) {
      // Nix would exit with 1, but this is a truly exceptional error, so we return -1
      return -1;
    }
  }|]
