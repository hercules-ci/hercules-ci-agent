#pragma once

#if NIX_IS_AT_LEAST(2,28,0)
#include <nix/store/derivations.hh>
#else
#include "derivations.hh"
#endif

#include <hercules-ci-cnix/store.hxx>

typedef nix::Strings::iterator StringsIterator;
typedef nix::DerivationOutputs::iterator DerivationOutputsIterator;
typedef nix::StringPairs::iterator StringPairsIterator;
