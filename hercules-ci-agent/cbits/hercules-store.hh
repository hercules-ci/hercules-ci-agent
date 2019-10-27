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
#include "HsFFI.h"

using namespace nix;

class WrappingStore : public Store {
 public:
  ref<Store> wrappedStore;

  WrappingStore(const Params & params, ref<Store> storeToWrap);


  virtual ~WrappingStore();

  virtual std::string getUri() override;

protected:

  virtual bool isValidPathUncached(const Path & path) override;

public:

  virtual PathSet queryValidPaths(const PathSet & paths,
      SubstituteFlag maybeSubstitute = NoSubstitute) override;
  virtual PathSet queryAllValidPaths() override;

protected:
  virtual void queryPathInfoUncached(const Path & path,
        Callback<std::shared_ptr<ValidPathInfo>> callback) noexcept override;

public:

  virtual void queryReferrers(const Path & path,
      PathSet & referrers) override;

  virtual PathSet queryValidDerivers(const Path & path) override;

  virtual PathSet queryDerivationOutputs(const Path & path) override;

  virtual StringSet queryDerivationOutputNames(const Path & path) override;

  virtual Path queryPathFromHashPart(const string & hashPart) override;

  virtual PathSet querySubstitutablePaths(const PathSet & paths) override;

  virtual void querySubstitutablePathInfos(const PathSet & paths,
        SubstitutablePathInfos & infos) override;

  virtual bool wantMassQuery() override;

  virtual void addToStore(const ValidPathInfo & info, Source & narSource,
        RepairFlag repair = NoRepair, CheckSigsFlag checkSigs = CheckSigs,
        std::shared_ptr<FSAccessor> accessor = 0) override;

  virtual void addToStore(const ValidPathInfo & info, const ref<std::string> & nar,
        RepairFlag repair = NoRepair, CheckSigsFlag checkSigs = CheckSigs,
        std::shared_ptr<FSAccessor> accessor = 0) override;

  virtual Path addToStore(const string & name, const Path & srcPath,
        bool recursive = true, HashType hashAlgo = htSHA256,
        PathFilter & filter = defaultPathFilter, RepairFlag repair = NoRepair) override;

  virtual Path addTextToStore(const string & name, const string & s,
        const PathSet & references, RepairFlag repair = NoRepair) override;

  virtual void narFromPath(const Path & path, Sink & sink) override;

  virtual void buildPaths(const PathSet & paths, BuildMode buildMode = bmNormal) override;

  virtual BuildResult buildDerivation(const Path & drvPath, const BasicDerivation & drv,
        BuildMode buildMode = bmNormal) override;

  virtual void ensurePath(const Path & path) override;

  virtual void addTempRoot(const Path & path) override;

  virtual void addIndirectRoot(const Path & path) override;

  virtual void syncWithGC() override;

  virtual void collectGarbage(const GCOptions & options, GCResults & results) override;

  virtual void optimiseStore() override;

  virtual bool verifyStore(bool checkContents, RepairFlag repair = NoRepair) override;

  virtual ref<FSAccessor> getFSAccessor() override;

  virtual void addSignatures(const Path & storePath, const StringSet & sigs) override;

    
  virtual void computeFSClosure(const PathSet & paths,
      PathSet & out, bool flipDirection = false,
      bool includeOutputs = false, bool includeDerivers = false) override;

  virtual void queryMissing(const PathSet & targets,
        PathSet & willBuild, PathSet & willSubstitute, PathSet & unknown,
        unsigned long long & downloadSize, unsigned long long & narSize) override;

  virtual std::shared_ptr<std::string> getBuildLog(const Path & path) override;

  virtual void connect() override;

  virtual int getPriority() override;

  virtual Path toRealPath(const Path & storePath) override;
};

class HerculesStore : public WrappingStore {
public:
  PathSet ensuredPaths;
  void (* builderCallback)(const char *, std::exception_ptr *exceptionToThrow);

  HerculesStore(const Params & params, ref<Store> storeToWrap);

  // Overrides

  virtual void ensurePath(const Path & path) override;

  virtual void buildPaths(const PathSet & paths, BuildMode buildMode = bmNormal) override;

  virtual BuildResult buildDerivation(const Path & drvPath, const BasicDerivation & drv,
        BuildMode buildMode = bmNormal) override;

  virtual void queryMissing(const PathSet& targets,
                            PathSet& willBuild,
                            PathSet& willSubstitute,
                            PathSet& unknown,
                            unsigned long long& downloadSize,
                            unsigned long long& narSize) override;

  // Additions

  void printDiagnostics();

  void setBuilderCallback(void (* newBuilderCallback)(const char *, std::exception_ptr *exceptionToThrow));

  void inhibitBuilds();
  void uninhibitBuilds();
};
