{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.Agent.Worker.Error where

import Data.ByteString.Unsafe (unsafePackMallocCString)
import Foreign (alloca, peek)
import Hercules.CNix.Store (traverseNonNull)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Protolude

C.context (C.cppCtx <> C.fptrCtx)

C.include "<nix/error.hh>"
C.include "<nix/util.hh>"
C.include "<iostream>"
C.include "<sstream>"

renderException :: SomeException -> IO (Text, Maybe Text)
renderException e | Just (C.CppStdException ex _msg _ty) <- fromException e = renderStdException ex
renderException e
  | Just (C.CppNonStdException _ex maybeType) <- fromException e =
    pure ("Unexpected C++ exception" <> foldMap (\t -> " of type " <> decodeUtf8With lenientDecode t) maybeType, Nothing)
renderException e | Just (FatalError msg) <- fromException e = pure (msg, Nothing)
renderException e = pure (toS $ displayException e, Nothing)

renderStdException :: C.CppExceptionPtr -> IO (Text, Maybe Text)
renderStdException e = alloca \traceStrPtr -> do
  msg <-
    [C.throwBlock| char * {
    char **traceStrPtr = $(char **traceStrPtr);
    *traceStrPtr = nullptr;
    std::string r;
    std::exception_ptr *e = $fptr-ptr:(std::exception_ptr *e);
    try {
      std::rethrow_exception(*e);
    } catch (const nix::Error &e) {
      {
        std::stringstream s;
        nix::showErrorInfo(s, e.info(), false);
        r = s.str();
      }
      {
        std::stringstream s;
        nix::showErrorInfo(s, e.info(), true);
        std::string t = s.str();
        // starts with r?
        if (t.rfind(r, 0) == 0) {
          t.replace(0, r.size(), "");
          t = nix::trim(t);
        }
        *traceStrPtr = strdup(t.c_str());
      }
    } catch (const std::exception &e) {
      r = e.what();
    } catch (...) {
      // shouldn't happen because inline-c-cpp only put std::exception in CppStdException
      throw std::runtime_error("renderStdException: Attempt to render unknown exception.");
    }

    return strdup(r.c_str());
  }|]
      >>= unsafePackMallocCString
      <&> decodeUtf8With lenientDecode
  traceText <-
    peek traceStrPtr >>= traverseNonNull \s ->
      unsafePackMallocCString s <&> decodeUtf8With lenientDecode
  pure (msg, traceText)
