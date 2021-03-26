#pragma once

#include "path.hh"
#include "types.hh"
#include "hash.hh"
#include "content-address.hh"
#include "sync.hh"

#include <map>
#include <variant>

namespace nix {
/* Abstract syntax of derivations. */

/* The traditional non-fixed-output derivation type. */
struct DerivationOutputInputAddressed
{
    StorePath path;
};

/* Fixed-output derivations, whose output paths are content addressed
   according to that fixed output. */
struct DerivationOutputCAFixed
{
    std::string drvName; // !!! not in Nix 2.4, which stores name in Derivation.
    FixedOutputHash hash; /* hash used for expected hash computation */
    StorePath path(const Store & store, std::string_view drvName, std::string_view outputName) const;
};

/* Floating-output derivations, whose output paths are content addressed, but
   not fixed, and so are dynamically calculated from whatever the output ends
   up being. */
struct DerivationOutputCAFloating
{
    /* information used for expected hash computation */
    FileIngestionMethod method;
    HashType hashType;
};

/* Input-addressed output which depends on a (CA) derivation whose hash isn't
 * known atm
 */
struct DerivationOutputDeferred {};

}

namespace compat::nix {

using namespace ::nix;

struct DerivationOutput
{
    std::variant<
        DerivationOutputInputAddressed,
        DerivationOutputCAFixed,
        DerivationOutputCAFloating,
        DerivationOutputDeferred
    > output;

    /* Note, when you use this function you should make sure that you're passing
       the right derivation name. When in doubt, you should use the safer
       interface provided by BasicDerivation::outputsAndOptPaths */
    std::optional<StorePath> path(const Store & store, std::string_view drvName, std::string_view outputName) const;
};

typedef std::map<string, DerivationOutput> DerivationOutputs;

}
