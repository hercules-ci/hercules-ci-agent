#include <cstdio>
#include <cstring>
#include <math.h>

#include <nix/util/util.hh>
#include <nix/store/derivations.hh>
#include <nix/store/path-with-outputs.hh>
#include <nix/store/store-api.hh>

#include "HsFFI.h"

using FSAccessor = nix::SourceAccessor;


using namespace nix;

class WrappingStore : public Store {
 public:
  ref<Store> wrappedStore;

  WrappingStore(ref<Store> storeToWrap);


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

    virtual StorePath addToStore(
        std::string_view name,
        const SourcePath & path,
        ContentAddressMethod method,
        HashAlgorithm hashAlgo,
        const StorePathSet & references,
        PathFilter & filter,
        RepairFlag repair) override;

    virtual StorePath addToStoreFromDump(
        Source & dump,
        std::string_view name,
        FileSerialisationMethod dumpMethod,
        ContentAddressMethod hashMethod,
        HashAlgorithm hashAlgo,
        const StorePathSet & references,
        RepairFlag repair) override;

  virtual void narFromPath(const StorePath & path, Sink & sink) override;

  virtual void buildPaths(
      const std::vector<DerivedPath> & paths,
      BuildMode buildMode = bmNormal,
      std::shared_ptr<Store> evalStore = nullptr) override;

  virtual BuildResult buildDerivation(const StorePath & drvPath, const BasicDerivation & drv,
      BuildMode buildMode = bmNormal) override;

  virtual void ensurePath(const StorePath & path) override;

  virtual void addTempRoot(const StorePath & path) override;



  virtual void optimiseStore() override;

  virtual bool verifyStore(bool checkContents, RepairFlag repair = NoRepair) override;

  virtual ref<FSAccessor> getFSAccessor(bool requireValidPath) override;

  virtual void addSignatures(const StorePath & storePath, const StringSet & sigs) override;

  virtual void computeFSClosure(const StorePathSet & paths,
      StorePathSet & out, bool flipDirection = false,
      bool includeOutputs = false, bool includeDerivers = false) override;

#if NIX_IS_AT_LEAST(2, 30, 0)
  virtual MissingPaths queryMissing(const std::vector<DerivedPath> & targets) override;
#else
  virtual void queryMissing(const std::vector<DerivedPath> & targets,
      StorePathSet & willBuild, StorePathSet & willSubstitute, StorePathSet & unknown,
      uint64_t & downloadSize, uint64_t & narSize) override;
#endif


  virtual unsigned int getProtocol() override;

  virtual void connect() override;

  virtual Path toRealPath(const Path & storePath) override;


  virtual std::optional<TrustedFlag> isTrustedClient() override;

};

class HerculesStore final : public WrappingStore {
public:
  StorePathSet ensuredPaths;
  void (* builderCallback)(std::vector<nix::StorePathWithOutputs>*, std::exception_ptr *exceptionToThrow);

  HerculesStore(ref<Store> storeToWrap);

#if !NIX_IS_AT_LEAST(2, 29, 0)
  virtual const std::string name() override;
#endif

  // Overrides

  virtual void queryRealisationUncached(const DrvOutput &,
        Callback<std::shared_ptr<const Realisation>> callback) noexcept override;

  virtual void ensurePath(const StorePath & path) override;

  virtual void buildPaths(
      const std::vector<DerivedPath> & paths,
      BuildMode buildMode = bmNormal,
      std::shared_ptr<Store> evalStore = nullptr) override;

  virtual BuildResult buildDerivation(const StorePath & drvPath, const BasicDerivation & drv,
      BuildMode buildMode = bmNormal) override;

#if NIX_IS_AT_LEAST(2, 30, 0)
  virtual MissingPaths queryMissing(const std::vector<DerivedPath> & targets) override;
#else
  virtual void queryMissing(const std::vector<DerivedPath> & targets,
      StorePathSet & willBuild, StorePathSet & willSubstitute, StorePathSet & unknown,
      uint64_t & downloadSize, uint64_t & narSize) override;
#endif

  // Additions

  void printDiagnostics();

  void setBuilderCallback(void (* newBuilderCallback)(std::vector<nix::StorePathWithOutputs>*, std::exception_ptr *exceptionToThrow));

  void inhibitBuilds();
  void uninhibitBuilds();
};
