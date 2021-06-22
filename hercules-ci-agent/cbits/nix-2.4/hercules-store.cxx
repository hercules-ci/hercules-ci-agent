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
#include <nix/affinity.hh>
#include <nix/globals.hh>
#include <nix/callback.hh>

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

StorePath WrappingStore::addToStore(const string & name, const Path & srcPath,
      FileIngestionMethod method, HashType hashAlgo,
      PathFilter & filter, RepairFlag repair) {
  return wrappedStore->addToStore(name, srcPath, method, hashAlgo, filter, repair);
}

StorePath WrappingStore::addToStoreFromDump(Source & dump, const string & name,
      FileIngestionMethod method, HashType hashAlgo, RepairFlag repair) {
  return wrappedStore->addToStoreFromDump(dump, name, method, hashAlgo, repair);
}

StorePath WrappingStore::addTextToStore(const string & name, const string & s,
      const StorePathSet & references, RepairFlag repair) {
  return wrappedStore->addTextToStore(name, s, references, repair);
}

void WrappingStore::narFromPath(const StorePath& path, Sink& sink) {
  wrappedStore->narFromPath(path, sink);
}

void WrappingStore::buildPaths(
      const std::vector<DerivedPath> & paths, BuildMode buildMode) {
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

void WrappingStore::addIndirectRoot(const Path& path) {
  wrappedStore->addIndirectRoot(path);
}

void WrappingStore::syncWithGC() {
  wrappedStore->syncWithGC();
}

void WrappingStore::collectGarbage(const GCOptions& options,
                                   GCResults& results) {
  wrappedStore->collectGarbage(options, results);
}

void WrappingStore::optimiseStore() {
  wrappedStore->optimiseStore();
};

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

std::shared_ptr<std::string> WrappingStore::getBuildLog(const StorePath& path) {
  return wrappedStore->getBuildLog(path);
}

void WrappingStore::connect() {
  wrappedStore->connect();
};

Path WrappingStore::toRealPath(const Path& storePath) {
  return wrappedStore->toRealPath(storePath);
};

Roots WrappingStore::findRoots(bool censor) {
  return wrappedStore->findRoots(censor);
}

unsigned int WrappingStore::getProtocol() {
  return wrappedStore->getProtocol();
}

void WrappingStore::createUser(const std::string & userName, uid_t userId) {
  wrappedStore->createUser(userName, userId);
}

/////

HerculesStore::HerculesStore(const Params& params, ref<Store> storeToWrap)
    : StoreConfig(params)
    , WrappingStore(params, storeToWrap) {}

const std::string HerculesStore::name() {
  return "wrapped " + wrappedStore->name();
}

std::optional<const Realisation> HerculesStore::queryRealisation(const DrvOutput &drvOut) {
  // TODO (ca-derivations) use?
  return wrappedStore->queryRealisation(drvOut);
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

void HerculesStore::buildPaths(const std::vector<DerivedPath> & derivedPaths, BuildMode buildMode) {
  std::exception_ptr exceptionToThrow(nullptr);

  // responsibility for delete is transferred to builderCallback
  auto pathsPtr = new std::vector<StorePathWithOutputs>();
  std::vector<StorePathWithOutputs> &paths = *pathsPtr;

  // TODO: don't ignore the Opaques
  for (auto & derivedPath : derivedPaths) {
    std::visit(overloaded {
      [&](DerivedPath::Built built) {
          paths.push_back(StorePathWithOutputs { built.drvPath, built.outputs });
      },
      [&](DerivedPath::Opaque opaque) {
          // TODO: pass this to the callback as well
      },
    }, derivedPath.raw());
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
