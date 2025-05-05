
#pragma once

#if NIX_IS_AT_LEAST(2,28,0)
#include <nix/store/path-info.hh>
#else
#include <nix/path-info.hh>
#endif

typedef nix::ref<nix::Store> refStore;

typedef nix::Strings::iterator StringsIterator;
typedef nix::DerivationOutputs::iterator DerivationOutputsIterator;
typedef nix::StringPairs::iterator StringPairsIterator;
typedef nix::PathSet::iterator PathSetIterator;
typedef nix::ref<const nix::ValidPathInfo> refValidPathInfo;

#if NIX_IS_AT_LEAST(2,18,0)
typedef nix::DerivedPathMap<std::set<nix::OutputName>>::Map::iterator DerivationInputsIterator;
#else
typedef nix::DerivationInputs::iterator DerivationInputsIterator;
#endif
