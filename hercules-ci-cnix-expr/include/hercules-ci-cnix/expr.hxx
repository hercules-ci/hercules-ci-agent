#pragma once

#include <nix/store/derivations.hh>
#include <nix/expr/value.hh>

#include <hercules-ci-cnix/store.hxx>

typedef nix::Strings::iterator StringsIterator;
typedef nix::DerivationOutputs::iterator DerivationOutputsIterator;
typedef nix::StringPairs::iterator StringPairsIterator;
#if NIX_IS_AT_LEAST(2, 30, 0)
typedef nix::ListView ListViewType;
#endif
