#include <nix/store-api.hh>
#include <nix/derivation-output.hh>
#include <nix/path-compat.hh>

// derivations.cc

inline void validatePath(std::string_view s) {
    if (s.size() == 0 || s[0] != '/')
        throw nix::FormatError("bad path '%1%' in derivation", s);
}

inline compat::nix::DerivationOutput parseDerivationOutput(const nix::Store & store, std::string drvName,
    std::string pathS, std::string hashAlgo, std::string hash)
{
    if (hashAlgo != "") {
        auto method = nix::FileIngestionMethod::Flat;
        if (std::string(hashAlgo, 0, 2) == "r:") {
            method = nix::FileIngestionMethod::Recursive;
            hashAlgo = hashAlgo.substr(2);
        }
        const auto hashType = nix::parseHashType(hashAlgo);
        if (hash != "") {
            validatePath(pathS);
            return compat::nix::DerivationOutput {
                .output = nix::DerivationOutputCAFixed {
                    .drvName = drvName,
                    .hash = nix::FixedOutputHash {
                        .method = std::move(method),
                        .hash = nix::Hash(hash, hashType),
                    },
                },
            };
        } else {
            throw nix::Error("ca-derivations not supported in Nix 2.3");
        }
    } else {
        if (pathS == "") {
            return compat::nix::DerivationOutput {
                .output = nix::DerivationOutputDeferred { }
            };
        }
        validatePath(pathS);
        return compat::nix::DerivationOutput {
            .output = nix::DerivationOutputInputAddressed {
                .path = compatParseStorePathStrict(store, pathS),
            }
        };
    }
}

inline compat::nix::DerivationOutput compatDerivationOutput(nix::Store &store, std::string drvName, nix::DerivationOutput &o) {
  return parseDerivationOutput(store, drvName, o.path, o.hashAlgo, o.hash);
}

inline void compatComputeFSClosure(nix::Store &store, nix::StorePathSet &pathSet, nix::StorePathSet &closurePaths, bool flipDirection = false, bool includeOutputs = false, bool includeDerivers = false) {
  nix::PathSet simpleClosurePaths;
  store.computeFSClosure(compatPathSet(store, pathSet), simpleClosurePaths, flipDirection, includeOutputs, includeDerivers);
  for (auto sp : simpleClosurePaths) {
    closurePaths.insert(compatParseStorePath(store, sp));
  }
}

inline nix::StorePathSet compatParseStorePathSet(const nix::Store &store, const nix::PathSet &pathSet) {
  nix::StorePathSet r;
  for (auto sp : pathSet) {
    r.insert(compatParseStorePath(store, sp));
  }
  return r;
}

// util.hh

// C++17 std::visit boilerplate
template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;
