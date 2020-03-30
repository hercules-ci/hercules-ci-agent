#pragma once

#include "hercules-store.hh"
#include "hercules-logger.hh"
// inline-c-cpp doesn't seem to handle namespace operator or template
// syntax so we help it a bit for now. This definition can be inlined
// when it is supported by inline-c-cpp.
typedef nix::ref<nix::Store> refStore;

typedef nix::ref<HerculesStore> refHerculesStore;

typedef nix::Logger::Fields LoggerFields;

typedef HerculesLogger::LogEntry HerculesLoggerEntry;
typedef std::queue<std::unique_ptr<HerculesLogger::LogEntry>> LogEntryQueue;

using namespace std;