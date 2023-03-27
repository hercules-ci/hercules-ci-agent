{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.Agent.Worker.Error where

import Data.ByteString.Unsafe (unsafePackMallocCString)
import Foreign (alloca, peek)
import Hercules.CNix.Encapsulation (HasEncapsulation (moveToForeignPtrWrapper))
import Hercules.CNix.Store (StorePath (StorePath), traverseNonNull)
import Hercules.CNix.Store.Context qualified
import Language.C.Inline.Cpp qualified as C
import Language.C.Inline.Cpp.Exception qualified as C
import Protolude

C.context (C.cppCtx <> C.fptrCtx <> C.bsCtx <> Hercules.CNix.Store.Context.context)

C.include "<hercules-error.hh>"
C.include "<nix/error.hh>"
C.include "<nix/util.hh>"
C.include "<iostream>"
C.include "<sstream>"

type ANSIText = Text

data ExceptionText = ExceptionText
  { exceptionTextMessage :: ANSIText,
    exceptionTextTrace :: Maybe ANSIText,
    exceptionTextDerivationPath :: Maybe StorePath
  }

throwBuildError :: Text -> StorePath -> IO a
throwBuildError msg drv = do
  let msgB = encodeUtf8 msg
  [C.throwBlock| void {
    std::string msg($bs-ptr:msgB, $bs-len:msgB);
    nix::StorePath &drv = *$fptr-ptr:(nix::StorePath *drv);
    throw hercules::HerculesBuildError(msg, drv);
  }|]
  panic "Could not throw HerculesBuildError!" -- tooling failure

basicExceptionText :: ANSIText -> ExceptionText
basicExceptionText msg =
  ExceptionText
    { exceptionTextMessage = msg,
      exceptionTextTrace = Nothing,
      exceptionTextDerivationPath = Nothing
    }

renderException :: SomeException -> IO ExceptionText
renderException e | Just (C.CppStdException ex _msg _ty) <- fromException e = renderStdException ex
renderException e
  | Just (C.CppNonStdException _ex maybeType) <- fromException e =
      pure $ basicExceptionText $ "Unexpected C++ exception" <> foldMap (\t -> " of type " <> decodeUtf8With lenientDecode t) maybeType
renderException e | Just (FatalError msg) <- fromException e = pure $ basicExceptionText msg
renderException e = pure $ basicExceptionText $ toS $ displayException e

renderStdException :: C.CppExceptionPtr -> IO ExceptionText
renderStdException e = alloca \traceStrPtr -> alloca \buildErrorDrvPtr -> do
  msg <-
    [C.throwBlock| const char * {
    const char **traceStrPtr = $(const char **traceStrPtr);
    *traceStrPtr = nullptr;

    nix::StorePath **buildErrorDrvPtr = $(nix::StorePath **buildErrorDrvPtr);
    *buildErrorDrvPtr = nullptr;

    std::exception_ptr *e = $fptr-ptr:(std::exception_ptr *e);
    try {
      std::rethrow_exception(*e);
    } catch (const hercules::HerculesBuildError &e) {
      *buildErrorDrvPtr = new nix::StorePath(e.drv);
      const char *msg;
      hercules::copyErrorStrings(e, &msg, traceStrPtr);
      return msg;
    } catch (const nix::Error &e) {
      const char *msg;
      hercules::copyErrorStrings(e, &msg, traceStrPtr);
      return msg;
    } catch (const std::exception &e) {
      return strdup(e.what());
    } catch (...) {
      // shouldn't happen because inline-c-cpp only put std::exception in CppStdException
      throw std::runtime_error("renderStdException: Attempt to render unknown exception.");
    }
  }|]
      >>= unsafePackMallocCString
      <&> decodeUtf8With lenientDecode
  traceText <-
    peek traceStrPtr >>= traverseNonNull \s ->
      unsafePackMallocCString s <&> decodeUtf8With lenientDecode
  drv <-
    peek buildErrorDrvPtr >>= traverseNonNull moveToForeignPtrWrapper
  pure $
    ExceptionText
      { exceptionTextMessage = msg,
        exceptionTextTrace = traceText,
        exceptionTextDerivationPath = drv
      }
