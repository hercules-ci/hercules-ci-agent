
#pragma once

#include <nix/store/path-info.hh>

typedef nix::ref<nix::Store> refStore;

typedef nix::Strings::iterator StringsIterator;
typedef nix::DerivationOutputs::iterator DerivationOutputsIterator;
typedef nix::StringPairs::iterator StringPairsIterator;
typedef nix::PathSet::iterator PathSetIterator;
typedef nix::ref<const nix::ValidPathInfo> refValidPathInfo;

typedef nix::DerivedPathMap<std::set<nix::OutputName>>::Map::iterator DerivationInputsIterator;
