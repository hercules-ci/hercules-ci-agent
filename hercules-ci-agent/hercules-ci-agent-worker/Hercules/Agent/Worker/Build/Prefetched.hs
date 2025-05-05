{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
#ifdef __GHCIDE__
# define NIX_IS_AT_LEAST(mm,m,p) 1
#endif

-- This implements an optimized routine to build from a remote derivation.
-- It is not in the "CNix" tree because it seems to be too specific for general use.
-- BuildStatus and BuildResult *can* be moved there, but I do not know of an
-- easy to maintain approach to do decouple it with inline-c-cpp. Perhaps it's
-- better to use an FFI generator instead?

module Hercules.Agent.Worker.Build.Prefetched
  ( buildDerivation,
    Hercules.Agent.Worker.Build.Prefetched.getDerivation,
    BuildResult (..),
  )
where

import Data.ByteString.Char8 qualified as C8
import Foreign (alloca, peek)
import Foreign.C (peekCString)
import Hercules.CNix.Encapsulation
import Hercules.CNix.Store
import Language.C.Inline.Cpp qualified as C
import Language.C.Inline.Cpp.Exception qualified as C
import Protolude

C.context context

C.include "<cstring>"

#if NIX_IS_AT_LEAST(2, 28, 0)

C.include "<nix/store/build-result.hh>"
C.include "<nix/store/derivations.hh>"
C.include "<nix/store/path-with-outputs.hh>"
C.include "<nix/store/store-api.hh>"
C.include "<nix/util/signals.hh>"

#else
C.include "<nix/config.h>"
C.include "<nix/shared.hh>"
C.include "<nix/store-api.hh>"
C.include "<nix/get-drvs.hh>"
C.include "<nix/derivations.hh>"
C.include "<nix/globals.hh>"
#  if NIX_IS_AT_LEAST(2,19,0)
C.include "<nix/signals.hh>"
#  else
-- redundant?
C.include "<nix/fs-accessor.hh>"
#  endif
#  if NIX_IS_AT_LEAST(2,7,0)
C.include "<nix/build-result.hh>"
#  endif

C.include "<nix/path-with-outputs.hh>"
#endif

C.include "<hercules-ci-cnix/store.hxx>"

C.include "<hercules-ci-cnix/string.hxx>"

C.using "namespace nix"

C.using "namespace hercules_ci_cnix"

data BuildStatus
  = Built
  | Substituted
  | AlreadyValid
  | PermanentFailure
  | InputRejected
  | OutputRejected
  | TransientFailure -- possibly transient
  | CachedFailure -- no longer used
  | TimedOut
  | MiscFailure
  | DependencyFailed
  | LogLimitExceeded
  | NotDeterministic
  | Successful -- Catch-all for unknown successful status
  | UnknownFailure -- Catch-all for unknown unsuccessful status
  deriving (Show)

-- Must match the FFI boilerplate
toBuildStatus :: C.CInt -> BuildStatus
toBuildStatus 0 = Built
toBuildStatus 1 = Substituted
toBuildStatus 2 = AlreadyValid
toBuildStatus 3 = PermanentFailure
toBuildStatus 4 = InputRejected
toBuildStatus 5 = OutputRejected
toBuildStatus 6 = TransientFailure
toBuildStatus 7 = CachedFailure
toBuildStatus 8 = TimedOut
toBuildStatus 9 = MiscFailure
toBuildStatus 10 = DependencyFailed
toBuildStatus 11 = LogLimitExceeded
toBuildStatus 12 = NotDeterministic
toBuildStatus (-1) = Successful
toBuildStatus _ = UnknownFailure

data BuildResult = BuildResult
  { isSuccess :: Bool,
    status :: BuildStatus,
    startTime :: C.CTime,
    stopTime :: C.CTime,
    errorMessage :: Text
  }
  deriving (Show)

getDerivation :: Store -> StorePath -> IO (Maybe Derivation)
getDerivation (Store store) derivationPath =
  nullableMoveToForeignPtrWrapper
    =<< [C.throwBlock| Derivation *{
      ReceiveInterrupts _;
      StorePath derivationPath = *$fptr-ptr:(nix::StorePath *derivationPath);
      std::list<nix::ref<nix::Store>> stores = getDefaultSubstituters();
      stores.push_front(*$(refStore* store));

      nix::Derivation *derivation = nullptr;

      for (nix::ref<nix::Store> & currentStore : stores) {
        try {
          derivation = new nix::Derivation(currentStore->derivationFromPath(derivationPath));
          break;
        } catch (nix::Interrupted &e) {
          throw e;
        } catch (nix::Error &e) {
          printTalkative("ignoring exception during drv lookup in %s: %s", currentStore->getUri(), e.what());
        } catch (std::exception &e) {
          printTalkative("ignoring exception during drv lookup in %s: %s", currentStore->getUri(), e.what());
        } catch (...) {
          // FIXME: remove this and make the "specific" catches above work on darwin
          printTalkative("ignoring unknown exception during drv lookup in %s: %s", currentStore->getUri());
        }
      }
      return derivation;
    }|]

-- | @buildDerivation derivationPath derivationText@
buildDerivation :: Store -> StorePath -> Derivation -> Maybe [ByteString] -> IO BuildResult
buildDerivation (Store store) derivationPath derivation extraInputs =
  let extraInputsMerged = C8.intercalate "\n" (fromMaybe [] extraInputs)
      materializeDerivation = if isNothing extraInputs then 1 else 0
   in alloca $ \successPtr ->
        alloca $ \statusPtr ->
          alloca $ \startTimePtr ->
            alloca $ \stopTimePtr ->
              alloca $ \errorMessagePtr -> do
                [C.throwBlock| void {
      ReceiveInterrupts _;
      Store &store = **$(refStore* store);
      bool &success = *$(bool *successPtr);
      int &status = *$(int *statusPtr);
      const char *&errorMessage = *$(const char **errorMessagePtr);
      time_t &startTime = *$(time_t *startTimePtr);
      time_t &stopTime = *$(time_t *stopTimePtr);
      StorePath derivationPath = *$fptr-ptr:(nix::StorePath *derivationPath);

      if ($(bool materializeDerivation)) {
        store.addTempRoot(derivationPath);
        store.ensurePath(derivationPath);
        auto derivation = store.derivationFromPath(derivationPath);
        StorePathWithOutputs storePathWithOutputs { .path = derivationPath, .outputs = derivation.outputNames() };
        std::vector<nix::StorePathWithOutputs> paths{storePathWithOutputs};
        try {
          store.buildPaths(toDerivedPaths(paths));
          status = -1;
          success = true;
          errorMessage = strdup("");
          startTime = 0;
          stopTime = 0;
        }
        catch (nix::Error &e) {
          printError(e.msg());
          status = -2;
          success = false;
          errorMessage = stringdup(e.msg());
          startTime = 0;
          stopTime = 0;
        }
      }
      else {
        nix::BasicDerivation *derivation = new BasicDerivation(*$fptr-ptr:(Derivation *derivation));
        std::string extraInputsMerged($bs-ptr:extraInputsMerged, $bs-len:extraInputsMerged);
        std::string extraInput;
        std::istringstream stream(extraInputsMerged);

        while (std::getline(stream, extraInput)) {
          auto path = store.parseStorePath(extraInput);
          derivation->inputSrcs.insert(path);
        }

        nix::BuildResult result = store.buildDerivation(derivationPath, *derivation);

        switch (result.status) {
          case nix::BuildResult::Built:
            status = 0;
            break;
          case nix::BuildResult::Substituted:
            status = 1;
            break;
          case nix::BuildResult::AlreadyValid:
            status = 2;
            break;
          case nix::BuildResult::PermanentFailure:
            status = 3;
            break;
          case nix::BuildResult::InputRejected:
            status = 4;
            break;
          case nix::BuildResult::OutputRejected:
            status = 5;
            break;
          case nix::BuildResult::TransientFailure: // possibly transient
            status = 6;
            break;
          case nix::BuildResult::CachedFailure: // no longer used
            status = 7;
            break;
          case nix::BuildResult::TimedOut:
            status = 8;
            break;
          case nix::BuildResult::MiscFailure:
            status = 9;
            break;
          case nix::BuildResult::DependencyFailed:
            status = 10;
            break;
          case nix::BuildResult::LogLimitExceeded:
            status = 11;
            break;
          case nix::BuildResult::NotDeterministic:
            status = 12;
            break;
          default:
            status = result.success() ? -1 : -2;
            break;
        }
        printError(result.errorMsg);
        success = result.success();
        errorMessage = stringdup(result.errorMsg);
        startTime = result.startTime;
        stopTime = result.stopTime;
      }
    }
    |]
                successValue <- peek successPtr
                statusValue <- peek statusPtr
                startTimeValue <- peek startTimePtr
                stopTimeValue <- peek stopTimePtr
                errorMessageValue0 <- peek errorMessagePtr
                errorMessageValue <- peekCString errorMessageValue0
                pure $
                  BuildResult
                    { isSuccess = successValue /= 0,
                      status = toBuildStatus statusValue,
                      startTime = startTimeValue,
                      stopTime = stopTimeValue,
                      errorMessage = toS errorMessageValue
                    }
