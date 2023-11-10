
#pragma once

#include <nix/path-info.hh>

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
