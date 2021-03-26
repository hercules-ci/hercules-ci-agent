
#include <nix/config.h>
#include <nix/store-api.hh>
#include <nix/derivations.hh>
#include <nix/path-compat.hh>
#include "nix/derivation-output.hh"

using namespace nix;

static std::string outputPathName(std::string_view drvName, std::string_view outputName) {
    std::string res { drvName };
    if (outputName != "out") {
        res += "-";
        res += outputName;
    }
    return res;
}

StorePath DerivationOutputCAFixed::path(const Store & store, std::string_view drvName, std::string_view outputName) const {
    return compatParseStorePathStrict(store, store.makeFixedOutputPath(
        hash.method == FileIngestionMethod::Recursive, hash.hash,
        outputPathName(drvName, outputName)));
}
