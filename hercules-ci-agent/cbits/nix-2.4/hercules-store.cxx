#include <cstdio>
#include <cstring>
#include <memory>
#include <math.h>
#include <nix/config.h>
#include <nix/shared.hh>
#include <nix/store-api.hh>
#include <nix/common-eval-args.hh>
#include <nix/get-drvs.hh>
#include <nix/derivations.hh>
#include <nix/globals.hh>
#include <nix/callback.hh>
#if NIX_IS_AT_LEAST(2,7,0)
#include <nix/build-result.hh>
#include <nix/gc-store.hh>
#endif
#include <nix/path-with-outputs.hh>

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
#if NIX_IS_AT_LEAST(2,7,0)
      std::string_view name,
#else
      const std::string & name,
#endif
      const Path & srcPath,
      FileIngestionMethod method, HashType hashAlgo,
      PathFilter & filter, RepairFlag repair
#if NIX_IS_AT_LEAST(2,5,0)
      , const StorePathSet & references
#endif
      ) {

  return wrappedStore->addToStore(name, srcPath, method, hashAlgo, filter, repair
#if NIX_IS_AT_LEAST(2,5,0)
      , references
#endif
  );

}

StorePath WrappingStore::addToStoreFromDump(
      Source & dump,
#if NIX_IS_AT_LEAST(2,7,0)
      std::string_view name,
#else
      const std::string & name,
#endif
      FileIngestionMethod method,
      HashType hashAlgo,
      RepairFlag repair
#if NIX_IS_AT_LEAST(2,5,0)
      , const StorePathSet & references
#endif
      ) {

  return wrappedStore->addToStoreFromDump(dump, name, method, hashAlgo, repair
#if NIX_IS_AT_LEAST(2,5,0)
      , references
#endif
  );
}

StorePath WrappingStore::addTextToStore(
#if NIX_IS_AT_LEAST(2,7,0)
      std::string_view name, std::string_view s,
#else
      const std::string & name, const std::string & s,
#endif
      const StorePathSet & references, RepairFlag repair) {
  return wrappedStore->addTextToStore(name, s, references, repair);
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

#if !NIX_IS_AT_LEAST(2,5,0)
void WrappingStore::syncWithGC() {
  wrappedStore->syncWithGC();
}
#endif

void WrappingStore::optimiseStore() {
  wrappedStore->optimiseStore();
};

#if !NIX_IS_AT_LEAST(2,7,0)
void WrappingStore::collectGarbage(const GCOptions& options,
                                   GCResults& results) {
  wrappedStore->collectGarbage(options, results);
}

void WrappingStore::addIndirectRoot(const Path& path) {
  wrappedStore->addIndirectRoot(path);
}

Roots WrappingStore::findRoots(bool censor) {
  return wrappedStore->findRoots(censor);
}
#endif

bool WrappingStore::verifyStore(bool checkContents, RepairFlag repair) {
  return wrappedStore->verifyStore(checkContents, repair);
};

ref<FSAccessor> WrappingStore::getFSAccessor() {
  return wrappedStore->getFSAccessor();
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

#if !NIX_IS_AT_LEAST(2,8,0)
#  if NIX_IS_AT_LEAST(2,6,0)
std::optional<std::string>
#  else
std::shared_ptr<std::string>
#  endif
WrappingStore::getBuildLog(const StorePath& path) {
  return wrappedStore->getBuildLog(path);
}
#endif

void WrappingStore::connect() {
  wrappedStore->connect();
};

Path WrappingStore::toRealPath(const Path& storePath) {
  return wrappedStore->toRealPath(storePath);
};

unsigned int WrappingStore::getProtocol() {
  return wrappedStore->getProtocol();
}

#if ! NIX_IS_AT_LEAST(2,14,0)
void WrappingStore::createUser(const std::string & userName, uid_t userId) {
  wrappedStore->createUser(userName, userId);
}
#endif

#if NIX_IS_AT_LEAST(2,15,0)
std::optional<TrustedFlag> WrappingStore::isTrustedClient() {
  return wrappedStore->isTrustedClient();
}
#endif

/////

HerculesStore::HerculesStore(const Params& params, ref<Store> storeToWrap)
    : StoreConfig(params)
    , WrappingStore(params, storeToWrap) {}

const std::string HerculesStore::name() {
  return "wrapped " + wrappedStore->name();
}

#if NIX_IS_AT_LEAST(2,5,0)
void HerculesStore::queryRealisationUncached(const DrvOutput &drvOutput,
  Callback<std::shared_ptr<const Realisation>> callback) noexcept {
  wrappedStore->queryRealisation(drvOutput, std::move(callback));
}
#else
std::optional<const Realisation> HerculesStore::queryRealisation(const DrvOutput &drvOut) {
  return wrappedStore->queryRealisation(drvOut);
}
#endif

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
#if NIX_IS_AT_LEAST(2,18,0)
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
#elif NIX_IS_AT_LEAST(2,13,0)
    auto sOrDrvPath = StorePathWithOutputs::tryFromDerivedPath(derivedPath);
    std::visit(overloaded {
        [&](const StorePathWithOutputs & s) {
            // paths.push_back(StorePathWithOutputs { built.drvPath, built.outputs });
            paths.push_back(s);
        },
        [&](const StorePath & drvPath) {
            // should this be substituted?
        },
    }, sOrDrvPath);
#else
    std::visit(overloaded {
      [&](DerivedPath::Built built) {
          paths.push_back(StorePathWithOutputs { built.drvPath, built.outputs });
      },
      [&](DerivedPath::Opaque opaque) {
          // TODO: pass this to the callback as well
      },
    }, derivedPath.raw());
#endif
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
