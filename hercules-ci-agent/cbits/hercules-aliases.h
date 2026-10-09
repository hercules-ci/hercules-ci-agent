#pragma once

#include "hercules-store.hh"
#include "hercules-logger.hh"
#include <hercules-ci-cnix/store.hxx>
#include <hercules-ci-cnix/expr.hxx>
#include <nix/store/derivations.hh>

// inline-c-cpp doesn't seem to handle namespace operator or template
// syntax so we help it a bit for now. This definition can be inlined
// when it is supported by inline-c-cpp.
typedef nix::ref<nix::Store> refStore;

typedef nix::ref<HerculesStore> refHerculesStore;

typedef nix::Logger::Fields LoggerFields;

typedef HerculesLogger::LogEntry HerculesLoggerEntry;
typedef std::queue<std::unique_ptr<HerculesLogger::LogEntry>> LogEntryQueue;

typedef nix::Strings::iterator StringsIterator;
typedef nix::DerivationOutputs::iterator DerivationOutputsIterator;
typedef nix::StringPairs::iterator StringPairsIterator;

#if NIX_IS_AT_LEAST(2, 34, 0)
#include "HaskellException.hxx"
#include <nix/expr/eval-error.hh>

/* Exception thrown by the builder callback into the evaluator.
 *
 * Nix >= 2.34 caches evaluation failures in thunks ("tFailed"). Deriving
 * from nix::RecoverableEvalError gives the failed thunk a recovery value,
 * so that forcing it again re-evaluates it, re-entering the builder
 * callback, needed for resuming non-blocking IFD.
 */
class HerculesBuilderException : public HaskellException, public nix::RecoverableEvalError
{
public:
  HerculesBuilderException(nix::EvalState & state, const std::string & renderedException, void *stablePtr)
    : HaskellException(renderedException, stablePtr)
    , nix::RecoverableEvalError(state, "%s", renderedException)
  {}
};
#endif

using namespace std;