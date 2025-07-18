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

using namespace std;