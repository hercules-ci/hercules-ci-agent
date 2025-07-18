#include <cstdio>
#include <cstring>
#include <memory>
#include <math.h>

#include <nix/store/build-result.hh>
#include <nix/store/derivations.hh>
#include <nix/store/globals.hh>
#include <nix/store/path-with-outputs.hh>
#include <nix/store/store-api.hh>
#include <nix/util/callback.hh>
#include <nix/expr/get-drvs.hh>
#include <nix/main/shared.hh>

#include "hercules-store.hh"

using namespace nix;

WrappingStore::WrappingStore(const Params& params, ref<Store> storeToWrap)
    : Store(params), wrappedStore(storeToWrap) {}

WrappingStore::~WrappingStore() {}

std::string WrappingStore::getUri() {
  return "wrapped:" + wrappedStore->getUri();
};

bool WrappingStore::isValidPathUncached(const StorePath & path) {
  return wrappedStore->isValidPath(path);  // caches again. Not much we can do.
}

StorePathSet WrappingStore::queryValidPaths(const StorePathSet& paths,
                                       SubstituteFlag maybeSubstitute) {
  return wrappedStore->queryValidPaths(paths, maybeSubstitute);
}
StorePathSet WrappingStore::queryAllValidPaths() {
  return wrappedStore->queryAllValidPaths();
}

// protected:
void WrappingStore::queryPathInfoUncached(const StorePath & path,
      Callback<std::shared_ptr<const ValidPathInfo>> callback) noexcept {

  auto callbackPtr = std::make_shared<decltype(callback)>(std::move(callback));

  wrappedStore->queryPathInfo(path, {[=](std::future<ref<const ValidPathInfo>> vpi){
    (*callbackPtr)(vpi.get().get_ptr());
  }});
}

// public:

void WrappingStore::queryReferrers(const StorePath& path, StorePathSet& referrers) {
  wrappedStore->queryReferrers(path, referrers);
}

StorePathSet WrappingStore::queryValidDerivers(const StorePath& path) {
  return wrappedStore->queryValidDerivers(path);
}

StorePathSet WrappingStore::queryDerivationOutputs(const StorePath& path) {
  return wrappedStore->queryDerivationOutputs(path);
}

std::optional<StorePath> WrappingStore::queryPathFromHashPart(const std::string & hashPart) {
  return wrappedStore->queryPathFromHashPart(hashPart);
}

StorePathSet WrappingStore::querySubstitutablePaths(const StorePathSet& paths) {
  return wrappedStore->querySubstitutablePaths(paths);
}

void WrappingStore::querySubstitutablePathInfos(const StorePathCAMap & paths,
      SubstitutablePathInfos & infos) {
  wrappedStore->querySubstitutablePathInfos(paths, infos);
}

void WrappingStore::addToStore(const ValidPathInfo & info, Source & narSource,
    RepairFlag repair, CheckSigsFlag checkSigs) {
  wrappedStore->addToStore(info, narSource, repair, checkSigs);
}

StorePath WrappingStore::addToStore(
    std::string_view name,
    const SourcePath & path,
    ContentAddressMethod method,
    HashAlgorithm hashAlgo,
    const StorePathSet & references,
    PathFilter & filter,
    RepairFlag repair)
{
    return wrappedStore->addToStore(name, path, method, hashAlgo, references, filter, repair);
}

StorePath WrappingStore::addToStoreFromDump(
    Source & dump,
    std::string_view name,
    FileSerialisationMethod dumpMethod,
    ContentAddressMethod method,
    HashAlgorithm hashAlgo,
    const StorePathSet & references,
    RepairFlag repair)
{
    return wrappedStore->addToStoreFromDump(dump, name, dumpMethod, method, hashAlgo, references, repair);
}

void WrappingStore::narFromPath(const StorePath& path, Sink& sink) {
  wrappedStore->narFromPath(path, sink);
}

void WrappingStore::buildPaths(
      const std::vector<DerivedPath> & paths, BuildMode buildMode,
      std::shared_ptr<Store> evalStore) {
  wrappedStore->buildPaths(paths, buildMode);
}

BuildResult WrappingStore::buildDerivation(const StorePath& drvPath,
                                           const BasicDerivation& drv,
                                           BuildMode buildMode) {
  return wrappedStore->buildDerivation(drvPath, drv, buildMode);
}

void WrappingStore::ensurePath(const StorePath& path) {
  wrappedStore->ensurePath(path);
}

void WrappingStore::addTempRoot(const StorePath& path) {
  wrappedStore->addTempRoot(path);
}

void WrappingStore::optimiseStore() {
  wrappedStore->optimiseStore();
};

bool WrappingStore::verifyStore(bool checkContents, RepairFlag repair) {
  return wrappedStore->verifyStore(checkContents, repair);
};

ref<FSAccessor> WrappingStore::getFSAccessor(bool requireValidPath) {
  return wrappedStore->getFSAccessor(requireValidPath);
}

void WrappingStore::addSignatures(const StorePath& storePath,
                                  const StringSet& sigs) {
  wrappedStore->addSignatures(storePath, sigs);
};

void WrappingStore::computeFSClosure(const StorePathSet& paths,
                                     StorePathSet& out,
                                     bool flipDirection,
                                     bool includeOutputs,
                                     bool includeDerivers) {
  wrappedStore->computeFSClosure(paths, out, flipDirection, includeOutputs,
                                 includeDerivers);
}

void WrappingStore::queryMissing(const std::vector<DerivedPath> & targets,
      StorePathSet & willBuild, StorePathSet & willSubstitute, StorePathSet & unknown,
      uint64_t & downloadSize, uint64_t & narSize) {
  wrappedStore->queryMissing(targets, willBuild, willSubstitute, unknown,
                             downloadSize, narSize);
}


void WrappingStore::connect() {
  wrappedStore->connect();
};

Path WrappingStore::toRealPath(const Path& storePath) {
  return wrappedStore->toRealPath(storePath);
};

unsigned int WrappingStore::getProtocol() {
  return wrappedStore->getProtocol();
}


std::optional<TrustedFlag> WrappingStore::isTrustedClient() {
  return wrappedStore->isTrustedClient();
}

/////

HerculesStore::HerculesStore(const Params& params, ref<Store> storeToWrap)
    : StoreConfig(params)
    , WrappingStore(params, storeToWrap) {}

const std::string HerculesStore::name() {
  return "wrapped " + wrappedStore->name();
}

void HerculesStore::queryRealisationUncached(const DrvOutput &drvOutput,
  Callback<std::shared_ptr<const Realisation>> callback) noexcept {
  wrappedStore->queryRealisation(drvOutput, std::move(callback));
}

void HerculesStore::ensurePath(const StorePath& path) {
  /* We avoid asking substituters for paths, since
     those would yield negative pathInfo caches on remote store.

     Instead, we only assert if path exists in the store.

     Once IFD build is performed, we ask for substitution
     via ensurePath.
  */
  if (!wrappedStore->isValidPath(path)) {
    std::exception_ptr exceptionToThrow(nullptr);
    // FIXME probably need this
    // builderCallback(path, &exceptionToThrow);
    if (exceptionToThrow != nullptr) {
      std::rethrow_exception(exceptionToThrow);
    }
    wrappedStore->ensurePath(path);
  }
  ensuredPaths.insert(path);
};

// Avoid substituting in evaluator, see `ensurePath` for more details
void HerculesStore::queryMissing(const std::vector<DerivedPath> & targets,
      StorePathSet & willBuild, StorePathSet & willSubstitute, StorePathSet & unknown,
      uint64_t & downloadSize, uint64_t & narSize) {
};

void HerculesStore::buildPaths(const std::vector<DerivedPath> & derivedPaths, BuildMode buildMode, std::shared_ptr<Store> evalStore) {
  std::exception_ptr exceptionToThrow(nullptr);

  // responsibility for delete is transferred to builderCallback
  auto pathsPtr = new std::vector<StorePathWithOutputs>();
  std::vector<StorePathWithOutputs> &paths = *pathsPtr;

  // TODO: don't ignore the Opaques
  for (auto & derivedPath : derivedPaths) {
    std::visit(overloaded {
        [&](const DerivedPathBuilt & b) {
            // TODO (RFC 92)
            // for now, we only support the non-inductive case
            std::visit(overloaded {
                [&](SingleDerivedPath::Opaque opaque) {
                    std::visit(overloaded {
                        [&](const OutputsSpec::All &) {
                            paths.emplace_back(StorePathWithOutputs {
                                .path = opaque.path,
                                .outputs = {}
                            });
                        },
                        [&](const OutputsSpec::Names & outs) {
                            paths.emplace_back(StorePathWithOutputs {
                                .path = opaque.path,
                                .outputs = outs
                            });
                        },
                    }, b.outputs.raw);
                },
                [&](SingleDerivedPath::Built built) {
                    throw nix::Error("hercules-ci-agent/buildPaths does not yet support dynamic derivation builds (outputOf)");
                },
            }, *b.drvPath);
        },
        [&](const DerivedPathOpaque & drvPath) {
            // should this be substituted?
        },
    }, derivedPath);
  }

  builderCallback(pathsPtr, &exceptionToThrow);

  if (exceptionToThrow != nullptr) {
    std::rethrow_exception(exceptionToThrow);
  }
}

BuildResult HerculesStore::buildDerivation(const StorePath & drvPath, const BasicDerivation & drv,
    BuildMode buildMode) {

  unsupported("buildDerivation");
  // unreachable
  return wrappedStore->buildDerivation(drvPath, drv, buildMode);
}

void HerculesStore::printDiagnostics() {
  for (auto path : ensuredPaths) {
    std::cerr << path.to_string() << std::endl;
  }
}

void HerculesStore::setBuilderCallback(void (* newBuilderCallback)(std::vector<nix::StorePathWithOutputs>*, std::exception_ptr *exceptionToThrow)) {
  builderCallback = newBuilderCallback;
}
