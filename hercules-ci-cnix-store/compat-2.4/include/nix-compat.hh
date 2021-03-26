#pragma once
#include <nix/store-api.hh>
#include <nix/derivations.hh>

#define compatPath(store, path) (path)
#define compatOutputPathSet(store, paths) (paths)
#define compatComputeFSClosure(src, pathSet, closurePaths) (src->computeFSClosure(pathSet, closurePaths))

namespace nix {
  inline StorePathWithOutputs parsePathWithOutputs(const Store &store, const std::string & s) {
    store.parsePathWithOutputs(s);
  }
}
