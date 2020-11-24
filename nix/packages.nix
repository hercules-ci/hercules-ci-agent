{ haskellPackages
, haskell
, pkgs
, nix
, ...
}:

let
  haskellPackages_ = haskellPackages;
  inherit (pkgs) recurseIntoAttrs lib;
  inherit (pkgs.lib) cleanSource makeBinPath optionalAttrs;
  inherit (haskell.lib)
    addBuildDepends
    addBuildTool
    addSetupDepends
    appendPatch
    buildFromSdist
    disableLibraryProfiling
    doJailbreak
    overrideCabal
    overrideSrc
    ;
  inherit (import sources.gitignore { inherit lib; }) gitignoreSource;
  callPkg = super: name: srcPath: args: overrideSrc (super.callCabal2nix name srcPath args) { src = gitignoreSource srcPath; };

  sources = import ./sources.nix;

  updateTo = v: stdPkg: altPkg:
    if lib.versionAtLeast stdPkg.version v
    then stdPkg
    else altPkg;

  internal =
    rec {
      inherit pkgs;

      # TODO: upstream the overrides
      haskellPackages =
        haskellPackages_.extend (
          self: super:
            {
              # 2020-11-21: cachix + chachix-api needs a patch for ghc 8.10 compat
              # https://github.com/cachix/cachix/pull/331
              cachix =
                updateTo "0.5.1" super.cachix (
                appendPatch (
                  haskell.lib.disableLibraryProfiling (
                    self.callPackage ./cachix.nix {}
                  )
                ) (
                  pkgs.fetchpatch {
                    url = https://github.com/cachix/cachix/commit/bfeec151a03afad72401815fe8bbb1b0d5d63b0d.patch;
                    sha256 = "1gma92966film44wfvb9dz86y82bih2ag6c5gj84dh1879ipmpdr";
                    stripLen = 2;
                    extraPrefix = "";
                    excludes = [ "stack.yaml" "sources.json" "cachix.cabal" "src/Cachix/Types/Session.hs" "src/Cachix/API/Signing.hs" "cachix-api.cabal" "workflows/test.yml" ];
                  }
                ));
              cachix-api =
                updateTo "0.5.0" super.cachix-api (
                appendPatch (self.callPackage ./cachix-api.nix {}) (
                pkgs.fetchpatch {
                  url = https://github.com/cachix/cachix/commit/bfeec151a03afad72401815fe8bbb1b0d5d63b0d.patch;
                  sha256 = "0rglyd77g4j72l5g0sj9zpq2hy3v992bm6nhj58pmj4j2aj67y74";
                  stripLen = 2;
                  extraPrefix = "";
                  includes = [ "src/Cachix/Types/Session.hs" "src/Cachix/API/Signing.hs" ];
                }
                ));

              nix-narinfo = self.callPackage ./nix-narinfo.nix {};

              protolude =
                updateTo "0.3" super.protolude (super.callPackage ./protolude-0.3.nix {});
              servant-auth =
                updateTo "0.4" super.servant-auth (super.callPackage ./servant-auth-0.4.nix {});
              servant-auth-client =
                updateTo "0.4.1" super.servant-auth-client (super.callPackage ./servant-auth-client-0.4.1.nix {});
              servant-auth-server =
                updateTo "0.4.6" super.servant-auth-server (super.callPackage ./servant-auth-server-0.4.6.nix {});
              servant-auth-swagger =
                updateTo "0.2.10.1" super.servant-auth-swagger (super.callPackage ./servant-auth-swagger-0.2.10.1.nix {});
              dhall =
                updateTo "1.28" super.dhall (super.callPackage ./dhall-1.28.nix {});

              hercules-ci-api = callPkg super "hercules-ci-api" ../hercules-ci-api {};
              # hercules-ci-api-agent = (callPkg super "hercules-ci-api-agent" ../hercules-ci-api-agent {}).overrideScope (self: super: { Cabal = self.Cabal_3_2_1_0; });
              # hercules-ci-api-agent = lib.addBuildDepends (callPkg super "hercules-ci-api-agent" ../hercules-ci-api-agent {}) [self.Cabal_3_2_1_0];
              hercules-ci-api-agent = callPkg super "hercules-ci-api-agent" ../hercules-ci-api-agent {};
              hercules-ci-api-core = callPkg super "hercules-ci-api-core" ../hercules-ci-api-core {};

              hercules-ci-agent =
                let
                  basePkg =
                    callPkg super "hercules-ci-agent" ../hercules-ci-agent {
                      bdw-gc = pkgs.boehmgc-hercules;
                    };

                in
                  buildFromSdist (
                    overrideCabal (
                      addBuildDepends
                        (addSetupDepends basePkg [ self.Cabal_3_2_1_0 ])
                        [ pkgs.makeWrapper pkgs.boost pkgs.boehmgc ]
                    ) (
                      o:
                        {
                          preCompileBuildDriver = ''
                            # setupCompileFlags+=" -clear-package-db -package-db ${self.ghcWithPackages (p: [self.Cabal_3_2_1_0])}/lib/*/package.conf.d"
                          '';
                          postCompileBuildDriver = ''
                            echo Setup version:
                            ./Setup --version
                          '';
                          
                          postInstall =
                            o.postInstall or ""
                            + ''
                              wrapProgram $out/bin/hercules-ci-agent --prefix PATH : ${makeBinPath [ pkgs.gnutar pkgs.gzip pkgs.git nix ]}
                            ''
                          ;
                          passthru =
                            (o.passthru or {})
                            // {
                              inherit nix;
                            }
                          ;

                          # TODO: We had an issue where any overrideCabal would have
                          #       no effect on the package, so we inline the
                          #       definition of justStaticExecutables here.
                          #       Ideally, we'd go back to a call to
                          #       justStaticExecutables, or even better,
                          #       a separate bin output.
                          #
                          # begin justStaticExecutables
                          enableSharedExecutables = false;
                          enableLibraryProfiling = false;
                          isLibrary = false;
                          doHaddock = false;
                          postFixup =
                            "rm -rf $out/lib $out/nix-support $out/share/doc";
                          # end justStaticExecutables
                        }
                    )
                  );

              hercules-ci-agent-test =
                callPkg super "hercules-ci-agent-test" ../tests/agent-test {};

              inline-c = self.callPackage ./haskell-inline-c.nix {};

              # avoid https://gitlab.haskell.org/ghc/ghc/issues/16477
              inline-c-cpp = overrideCabal (haskell.lib.disableLibraryProfiling (self.callPackage ./haskell-inline-c-cpp.nix {}))
                (
                  o: {
                    preConfigure = ''
                      substituteInPlace inline-c-cpp.cabal --replace " c++ " stdc++ 
                    '';
                  }
                );

              websockets = updateTo "0.12.6.1" super.websockets (self.callPackage ./websockets.nix {});

              servant-websockets = self.callPackage ./servant-websockets.nix {};

            }
        );

      hercules-ci-api-swagger =
        pkgs.callPackage ../hercules-ci-api/swagger.nix { inherit (haskellPackages) hercules-ci-api; };

      tests =
        recurseIntoAttrs {
          agent-functional-test = pkgs.nixosTest ../tests/agent-test.nix;
          module-nixos = pkgs.callPackage ../tests/module-nixos {};
        };

      projectRootSource = gitignoreSource ../.;
    };
in
recurseIntoAttrs {
  inherit (internal.haskellPackages) hercules-ci-agent;
  inherit (internal) hercules-ci-api-swagger;
  # isx86_64: Don't run the VM tests on aarch64 to save time
  tests = if pkgs.stdenv.isLinux && pkgs.stdenv.isx86_64 then internal.tests else null;
  pre-commit-check =
    (import sources."pre-commit-hooks.nix").run {
      src = ../.;
      tools = {
        inherit (pkgs) ormolu;
      };
      hooks = {
        # TODO: hlint.enable = true;
        #ormolu.enable = true;
        ormolu.excludes = [
          # CPP
          "Hercules/Agent/Compat.hs"
          "Hercules/Agent/StoreFFI.hs"
        ];
        #shellcheck.enable = true;
        #nixpkgs-fmt.enable = true;
        nixpkgs-fmt.excludes = [ "tests/agent-test/testdata/" ];
      };
      settings.ormolu.defaultExtensions = [ "TypeApplications" ];
    };

  # Not traversed for derivations:
  inherit internal;
}
