#pragma once
#include <nix/store-api.hh>
#include <nix/derivations.hh>
#include <nix/path-with-outputs.hh>

inline void compatComputeFSClosure(nix::Store &store, nix::StorePathSet &pathSet, nix::StorePathSet &closurePaths, bool flipDirection = false, bool includeOutputs = false, bool includeDerivers = false) {
  store.computeFSClosure(pathSet, closurePaths, flipDirection, includeOutputs, includeDerivers);
}
