#include <cstdio>
#include <cstring>
#include <math.h>
#include <nix/config.h>
#include <nix/shared.hh>
#include <nix/store-api.hh>
#include <nix/common-eval-args.hh>
#include <nix/get-drvs.hh>
#include <nix/derivations.hh>
#include <nix/affinity.hh>
#include <nix/globals.hh>

#include "hercules-store.hh"

using namespace nix;

WrappingStore::WrappingStore(const Params& params, ref<Store> storeToWrap)
    : Store(params), wrappedStore(storeToWrap) {}

WrappingStore::~WrappingStore() {}

std::string WrappingStore::getUri() {
  return "wrapped:" + wrappedStore->getUri();
};

bool WrappingStore::isValidPathUncached(const Path& path) {
  return wrappedStore->isValidPath(path);  // not ideal
}

PathSet WrappingStore::queryValidPaths(const PathSet& paths,
                                       SubstituteFlag maybeSubstitute) {
  return wrappedStore->queryValidPaths(paths, maybeSubstitute);
}
PathSet WrappingStore::queryAllValidPaths() {
  return wrappedStore->queryAllValidPaths();
}

// protected:
void WrappingStore::queryPathInfoUncached(
    const Path& path,
    Callback<std::shared_ptr<ValidPathInfo>> callback) noexcept {
  unsupported("queryPathInfoUncached");
  /*
      Callback<ref<ValidPathInfo>>
     callback2([&callback](std::future<ref<ValidPathInfo>> vpi){
        std::shared_ptr<ValidPathInfo> !@#$%^&*
        callback(vpi);
      });
      return wrappedStore->queryPathInfo(path, callback2); // not ideal
  */
}

// public:

void WrappingStore::queryReferrers(const Path& path, PathSet& referrers) {
  wrappedStore->queryReferrers(path, referrers);
}

PathSet WrappingStore::queryValidDerivers(const Path& path) {
  return wrappedStore->queryValidDerivers(path);
}

PathSet WrappingStore::queryDerivationOutputs(const Path& path) {
  return wrappedStore->queryDerivationOutputs(path);
}

StringSet WrappingStore::queryDerivationOutputNames(const Path& path) {
  return wrappedStore->queryDerivationOutputNames(path);
}

Path WrappingStore::queryPathFromHashPart(const string& hashPart) {
  return wrappedStore->queryPathFromHashPart(hashPart);
}

PathSet WrappingStore::querySubstitutablePaths(const PathSet& paths) {
  return wrappedStore->querySubstitutablePaths(paths);
}

void WrappingStore::querySubstitutablePathInfos(const PathSet& paths,
                                                SubstitutablePathInfos& infos) {
  wrappedStore->querySubstitutablePathInfos(paths, infos);
}

bool WrappingStore::wantMassQuery() {
  return wrappedStore->wantMassQuery();
}

void WrappingStore::addToStore(const ValidPathInfo& info,
                               Source& narSource,
                               RepairFlag repair,
                               CheckSigsFlag checkSigs,
                               std::shared_ptr<FSAccessor> accessor) {
  wrappedStore->addToStore(info, narSource, repair, checkSigs, accessor);
}

void WrappingStore::addToStore(const ValidPathInfo& info,
                               const ref<std::string>& nar,
                               RepairFlag repair,
                               CheckSigsFlag checkSigs,
                               std::shared_ptr<FSAccessor> accessor) {
  wrappedStore->addToStore(info, nar, repair, checkSigs, accessor);
}

Path WrappingStore::addToStore(const string& name,
                               const Path& srcPath,
                               bool recursive,
                               HashType hashAlgo,
                               PathFilter& filter,
                               RepairFlag repair) {
  return wrappedStore->addToStore(name, srcPath, recursive, hashAlgo, filter,
                                  repair);
}

Path WrappingStore::addTextToStore(const string& name,
                                   const string& s,
                                   const PathSet& references,
                                   RepairFlag repair) {
  return wrappedStore->addTextToStore(name, s, references, repair);
}

void WrappingStore::narFromPath(const Path& path, Sink& sink) {
  wrappedStore->narFromPath(path, sink);
}

void WrappingStore::buildPaths(const PathSet& paths, BuildMode buildMode) {
  wrappedStore->buildPaths(paths, buildMode);
}

BuildResult WrappingStore::buildDerivation(const Path& drvPath,
                                           const BasicDerivation& drv,
                                           BuildMode buildMode) {
  return wrappedStore->buildDerivation(drvPath, drv, buildMode);
}

void WrappingStore::ensurePath(const Path& path) {
  wrappedStore->ensurePath(path);
}

void WrappingStore::addTempRoot(const Path& path) {
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

void WrappingStore::addSignatures(const Path& storePath,
                                  const StringSet& sigs) {
  wrappedStore->addSignatures(storePath, sigs);
};

void WrappingStore::computeFSClosure(const PathSet& paths,
                                     PathSet& out,
                                     bool flipDirection,
                                     bool includeOutputs,
                                     bool includeDerivers) {
  wrappedStore->computeFSClosure(paths, out, flipDirection, includeOutputs,
                                 includeDerivers);
}

void WrappingStore::queryMissing(const PathSet& targets,
                                 PathSet& willBuild,
                                 PathSet& willSubstitute,
                                 PathSet& unknown,
                                 unsigned long long& downloadSize,
                                 unsigned long long& narSize) {
  wrappedStore->queryMissing(targets, willBuild, willSubstitute, unknown,
                             downloadSize, narSize);
}

std::shared_ptr<std::string> WrappingStore::getBuildLog(const Path& path) {
  return wrappedStore->getBuildLog(path);
}

void WrappingStore::connect() {
  wrappedStore->connect();
};

int WrappingStore::getPriority() {
  return wrappedStore->getPriority();
}

Path WrappingStore::toRealPath(const Path& storePath) {
  return wrappedStore->toRealPath(storePath);
};

/////

HerculesStore::HerculesStore(const Params& params, ref<Store> storeToWrap)
    : WrappingStore(params, storeToWrap) {}

void HerculesStore::ensurePath(const Path& path) {
  /* We avoid asking substituters for paths, since
     those would yield negative pathInfo caches on remote store.

     Instead, we only assert if path exists in the store.

     Once IFD build is performed, we ask for substitution
     via ensurePath.
  */
  if (!wrappedStore->isValidPath(path)) {
    std::exception_ptr exceptionToThrow(nullptr);
    builderCallback(strdup(path.c_str()), &exceptionToThrow);
    if (exceptionToThrow != nullptr) {
      std::rethrow_exception(exceptionToThrow);
    }
    wrappedStore->ensurePath(path);
  }
  ensuredPaths.insert(path);
};

// Avoid substituting in evaluator, see `ensurePath` for more details
void HerculesStore::queryMissing(const PathSet& targets,
                                 PathSet& willBuild,
                                 PathSet& willSubstitute,
                                 PathSet& unknown,
                                 unsigned long long& downloadSize,
                                 unsigned long long& narSize) {
};

void HerculesStore::buildPaths(const PathSet& paths, BuildMode buildMode) {
  for (Path path : paths) {
    std::exception_ptr exceptionToThrow(nullptr);
    builderCallback(strdup(path.c_str()), &exceptionToThrow);
    if (exceptionToThrow != nullptr) {
      std::rethrow_exception(exceptionToThrow);
    }
  }
}

BuildResult HerculesStore::buildDerivation(const Path& drvPath,
                                           const BasicDerivation& drv,
                                           BuildMode buildMode) {
  unsupported("buildDerivation");

  std::cerr << "building derivation " << drvPath << std::endl;
  auto r = wrappedStore->buildDerivation(drvPath, drv, buildMode);
  std::cerr << "built derivation " << drvPath << std::endl;
  return r;
}

void HerculesStore::printDiagnostics() {
  for (std::string path : ensuredPaths) {
    std::cerr << path << std::endl;
  }
}

void HerculesStore::setBuilderCallback(void (* newBuilderCallback)(const char *, std::exception_ptr *exceptionToThrow)) {
  builderCallback = newBuilderCallback;
}
