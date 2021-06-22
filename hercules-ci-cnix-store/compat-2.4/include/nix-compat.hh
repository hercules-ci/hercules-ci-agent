#pragma once
#include <nix/store-api.hh>
#include <nix/derivations.hh>
#include <nix/path-with-outputs.hh>

#define printPath23(store, path) (path)
#define compatPathSet(store, paths) (paths)
#define printPathSet23(store, pathSet) (pathSet)
#define parseStorePath23(store, path) (path)
#define parseStorePathSet23(store, pathSet) (pathSet)
#define parseOptionalStorePath23(store, path) (path)
#define toDerivedPaths24(x) toDerivedPaths(x)

inline nix::StorePath parseStorePath(const nix::Store &store, const nix::Path path) {
  return store.parseStorePath(path);
}

inline void compatComputeFSClosure(nix::Store &store, nix::StorePathSet &pathSet, nix::StorePathSet &closurePaths, bool flipDirection = false, bool includeOutputs = false, bool includeDerivers = false) {
  store.computeFSClosure(pathSet, closurePaths, flipDirection, includeOutputs, includeDerivers);
}

namespace nix {
  inline StorePathWithOutputs parsePathWithOutputs(Store &store, const std::string & s) {
    return parsePathWithOutputs(store, s);
  }
}
