
#pragma once

#include <nix/store/path-info.hh>
#if NIX_IS_AT_LEAST(2, 29, 0)
#include <nix/store/derived-path-map.hh>
#endif

typedef nix::ref<nix::Store> refStore;

typedef nix::Strings::iterator StringsIterator;
typedef nix::DerivationOutputs::iterator DerivationOutputsIterator;
typedef nix::StringPairs::iterator StringPairsIterator;
typedef nix::PathSet::iterator PathSetIterator;
typedef nix::ref<const nix::ValidPathInfo> refValidPathInfo;

// https://github.com/NixOS/nix/pull/13129
#if NIX_IS_AT_LEAST(2, 29, 0)
typedef nix::DerivedPathMap<nix::StringSet>::Map::iterator DerivationInputsIterator;
#else
typedef nix::DerivedPathMap<std::set<nix::OutputName>>::Map::iterator DerivationInputsIterator;
#endif
