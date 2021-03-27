#pragma once
#include "path.hh"
#include <vector>

inline nix::Path printPath23(const nix::Store &store, const nix::StorePath &sp) {
  return (store.storeDir + "/").append(sp.to_string());
}

inline nix::StorePath parseStorePath(const nix::Store &store, const nix::Path path) {
  nix::Path p = nix::canonPath(path);
  if (nix::dirOf(p) != store.storeDir)
    throw nix::Error("path '%s' is not in the Nix store", p);
  return nix::StorePath(nix::baseNameOf(p));
}

inline nix::PathSet printPathSet23(const nix::Store &store, std::vector<nix::StorePathWithOutputs> &sps) {
  nix::PathSet r;
  for (auto spwo : sps) {
    for (auto output : spwo.outputs) {
      r.insert(printPath23(store, spwo.path) + "!" + output);
    }
  }
  return r;
}

inline nix::PathSet compatPathSet(const nix::Store &store, nix::StorePathSet &sps) {
  nix::PathSet r;
  for (auto sp : sps) {
    r.insert(printPath23(store, sp));
  }
  return r;
}

inline nix::StorePathSet compatStorePathSet(const nix::Store &store, nix::PathSet &sps) {
  nix::StorePathSet r;
  for (auto sp : sps) {
    r.insert(parseStorePath(store, sp));
  }
  return r;
}

#define parseStorePath23(store, path) parseStorePath(store, path)
