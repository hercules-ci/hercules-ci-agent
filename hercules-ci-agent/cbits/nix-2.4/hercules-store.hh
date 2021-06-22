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
#include <nix-compat.hh>
#include "HsFFI.h"

using namespace nix;

class WrappingStore : public Store {
 public:
  ref<Store> wrappedStore;

  WrappingStore(const Params & params, ref<Store> storeToWrap);


  virtual ~WrappingStore();

  virtual std::string getUri() override;

protected:

  virtual bool isValidPathUncached(const StorePath & path) override;

public:

  virtual StorePathSet queryValidPaths(const StorePathSet & paths,
      SubstituteFlag maybeSubstitute = NoSubstitute) override;
  virtual StorePathSet queryAllValidPaths() override;

protected:
  virtual void queryPathInfoUncached(const StorePath & path,
      Callback<std::shared_ptr<const ValidPathInfo>> callback) noexcept override;

public:

  virtual void queryReferrers(const StorePath & path,
      StorePathSet & referrers) override;

  virtual StorePathSet queryValidDerivers(const StorePath & path) override;

  virtual StorePathSet queryDerivationOutputs(const StorePath & path) override;

  virtual std::optional<StorePath> queryPathFromHashPart(const std::string & hashPart) override;

  virtual StorePathSet querySubstitutablePaths(const StorePathSet & paths) override;

  virtual void querySubstitutablePathInfos(const StorePathCAMap & paths,
      SubstitutablePathInfos & infos) override;

  virtual void addToStore(const ValidPathInfo & info, Source & narSource,
      RepairFlag repair = NoRepair, CheckSigsFlag checkSigs = CheckSigs) override;

  virtual StorePath addToStore(const string & name, const Path & srcPath,
      FileIngestionMethod method = FileIngestionMethod::Recursive, HashType hashAlgo = htSHA256,
      PathFilter & filter = defaultPathFilter, RepairFlag repair = NoRepair) override;

  virtual StorePath addToStoreFromDump(Source & dump, const string & name,
      FileIngestionMethod method = FileIngestionMethod::Recursive, HashType hashAlgo = htSHA256, RepairFlag repair = NoRepair) override;

  virtual StorePath addTextToStore(const string & name, const string & s,
      const StorePathSet & references, RepairFlag repair = NoRepair) override;

  virtual void narFromPath(const StorePath & path, Sink & sink) override;

  virtual void buildPaths(
      const std::vector<DerivedPath> & paths,
      BuildMode buildMode = bmNormal) override;

  virtual BuildResult buildDerivation(const StorePath & drvPath, const BasicDerivation & drv,
      BuildMode buildMode = bmNormal) override;

  virtual void ensurePath(const StorePath & path) override;

  virtual void addTempRoot(const StorePath & path) override;

  virtual void addIndirectRoot(const Path & path) override;

  virtual void syncWithGC() override;

  virtual Roots findRoots(bool censor) override;

  virtual void collectGarbage(const GCOptions & options, GCResults & results) override;

  virtual void optimiseStore() override;

  virtual bool verifyStore(bool checkContents, RepairFlag repair = NoRepair) override;

  virtual ref<FSAccessor> getFSAccessor() override;

  virtual void addSignatures(const StorePath & storePath, const StringSet & sigs) override;

  virtual void computeFSClosure(const StorePathSet & paths,
      StorePathSet & out, bool flipDirection = false,
      bool includeOutputs = false, bool includeDerivers = false) override;

  virtual void queryMissing(const std::vector<DerivedPath> & targets,
      StorePathSet & willBuild, StorePathSet & willSubstitute, StorePathSet & unknown,
      uint64_t & downloadSize, uint64_t & narSize) override;

  virtual std::shared_ptr<std::string> getBuildLog(const StorePath & path) override;

  virtual unsigned int getProtocol() override;

  virtual void connect() override;

  virtual Path toRealPath(const Path & storePath) override;

  virtual void createUser(const std::string & userName, uid_t userId) override;

};

class HerculesStore : public WrappingStore {
public:
  StorePathSet ensuredPaths;
  void (* builderCallback)(std::vector<nix::StorePathWithOutputs>*, std::exception_ptr *exceptionToThrow);

  HerculesStore(const Params & params, ref<Store> storeToWrap);

  virtual const std::string name() override;

  // Overrides

  virtual std::optional<const Realisation> queryRealisation(const DrvOutput &) override;

  virtual void ensurePath(const StorePath & path) override;

  virtual void buildPaths(
      const std::vector<DerivedPath> & paths,
      BuildMode buildMode = bmNormal) override;

  virtual BuildResult buildDerivation(const StorePath & drvPath, const BasicDerivation & drv,
      BuildMode buildMode = bmNormal) override;

  virtual void queryMissing(const std::vector<DerivedPath> & targets,
      StorePathSet & willBuild, StorePathSet & willSubstitute, StorePathSet & unknown,
      uint64_t & downloadSize, uint64_t & narSize) override;

  // Additions

  void printDiagnostics();

  void setBuilderCallback(void (* newBuilderCallback)(std::vector<nix::StorePathWithOutputs>*, std::exception_ptr *exceptionToThrow));

  void inhibitBuilds();
  void uninhibitBuilds();
};
