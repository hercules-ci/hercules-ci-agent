
#pragma once

#include <nix/path-info.hh>

typedef nix::ref<nix::Store> refStore;

typedef nix::Strings::iterator StringsIterator;
typedef nix::DerivationOutputs::iterator DerivationOutputsIterator;
typedef nix::DerivationInputs::iterator DerivationInputsIterator;
typedef nix::StringPairs::iterator StringPairsIterator;
typedef nix::PathSet::iterator PathSetIterator;
typedef nix::ref<const nix::ValidPathInfo> refValidPathInfo;
