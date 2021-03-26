#pragma once
#include "path.hh"
#include <vector>

inline nix::Path compatPath(const nix::Store &store, const nix::StorePath &sp) {
  return (store.storeDir + "/").append(sp.to_string());
}

inline nix::StorePath compatParseStorePathStrict(const nix::Store &store, const nix::Path p) {
  if (nix::dirOf(p) != store.storeDir)
    throw nix::Error("path '%s' is not in the Nix store", p);
  return nix::StorePath(nix::baseNameOf(p));
}

inline nix::StorePath compatParseStorePath(const nix::Store &store, const nix::Path p) {
  return compatParseStorePathStrict(store, nix::canonPath(p));
}

inline nix::PathSet compatOutputPathSet(const nix::Store &store, std::vector<nix::StorePathWithOutputs> &sps) {
  nix::PathSet r;
  for (auto spwo : sps) {
    for (auto output : spwo.outputs) {
      r.insert(compatPath(store, spwo.path) + "!" + output);
    }
  }
  return r;
}

inline nix::PathSet compatPathSet(const nix::Store &store, nix::StorePathSet &sps) {
  nix::PathSet r;
  for (auto sp : sps) {
    r.insert(compatPath(store, sp));
  }
  return r;
}

inline nix::StorePathSet compatStorePathSet(const nix::Store &store, nix::PathSet &sps) {
  nix::StorePathSet r;
  for (auto sp : sps) {
    r.insert(compatParseStorePath(store, sp));
  }
  return r;
}
