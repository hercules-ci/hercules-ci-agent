{ haskellPackages
, haskell
, pkgs
, nix
  # TODO propagatedBuildInputs upstream
, nlohmann_json
, pre-commit-hooks-nix
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
    allowInconsistentDependencies
    appendPatch
    buildFromSdist
    disableLibraryProfiling
    enableDWARFDebugging
    doJailbreak
    generateOptparseApplicativeCompletion
    justStaticExecutables
    overrideCabal
    overrideSrc
    ;
  callPkg = super: name: srcPath: args: overrideSrc (super.callCabal2nix name srcPath args) { src = srcPath; };

  addNixVersionFlag = pkg:
    overrideCabal pkg (o: {
      preConfigure = (o.preConfigure or "") + ''
        if pkg-config --atleast-version 2.4pre nix-store; then
          configureFlags="$configureFlags --flag nix-2_4"
        fi
      '';
    });

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
              cachix = self.callPackage ./cachix.nix { };
              cachix-api =
                updateTo "0.6.0" super.cachix-api (self.callPackage ./cachix-api.nix { });

              nix-narinfo = self.callPackage ./nix-narinfo.nix { };

              hercules-ci-optparse-applicative =
                super.callPackage ./hercules-ci-optparse-applicative.nix { };
              protolude =
                updateTo "0.3" super.protolude (super.callPackage ./protolude-0.3.nix { });
              servant-auth =
                updateTo "0.4" super.servant-auth (super.callPackage ./servant-auth-0.4.nix { });
              servant-auth-client =
                updateTo "0.4.1" super.servant-auth-client (super.callPackage ./servant-auth-client-0.4.1.nix { });
              servant-auth-server =
                updateTo "0.4.6" super.servant-auth-server (super.callPackage ./servant-auth-server-0.4.6.nix { });
              servant-auth-swagger =
                updateTo "0.2.10.1" super.servant-auth-swagger (super.callPackage ./servant-auth-swagger-0.2.10.1.nix { });
              dhall =
                updateTo "1.28" super.dhall (super.callPackage ./dhall-1.28.nix { });

              hercules-ci-api = callPkg super "hercules-ci-api" ../hercules-ci-api { };
              hercules-ci-api-agent = callPkg super "hercules-ci-api-agent" ../hercules-ci-api-agent { };
              hercules-ci-api-core = callPkg super "hercules-ci-api-core" ../hercules-ci-api-core { };

              hercules-ci-agent =
                let
                  basePkg =
                    callPkg super "hercules-ci-agent" ../hercules-ci-agent {
                      bdw-gc = pkgs.boehmgc-hercules;
                    };
                  bundledBins = [ pkgs.gnutar pkgs.gzip pkgs.git nix ] ++ lib.optional pkgs.stdenv.isLinux pkgs.runc;

                in
                generateOptparseApplicativeCompletion "hercules-ci-agent" (buildFromSdist (
                  overrideCabal
                    (
                      addBuildDepends
                        (enableDWARFDebugging (addNixVersionFlag basePkg))
                        [ pkgs.makeWrapper pkgs.boost pkgs.boehmgc ]
                    )
                    (
                      o:
                      {
                        postCompileBuildDriver = ''
                          echo Setup version:
                          ./Setup --version
                        '';

                        postInstall =
                          o.postInstall or ""
                          + ''
                            wrapProgram $out/bin/hercules-ci-agent --prefix PATH : ${makeBinPath bundledBins}
                          ''
                        ;
                        passthru =
                          (o.passthru or { })
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
                ));

              hercules-ci-agent-test =
                callPkg super "hercules-ci-agent-test" ../tests/agent-test { };

              hercules-ci-cli = overrideCabal
                (
                  generateOptparseApplicativeCompletion "hci" (
                    justStaticExecutables (
                      haskell.lib.disableLibraryProfiling (
                        callPkg super "hercules-ci-cli" ../hercules-ci-cli {
                          hercules-ci-agent = overrideCabal self.hercules-ci-agent (o: {
                            isLibrary = true;
                            isExecutable = false;
                            postFixup = "";
                          });
                        }
                      )
                    )
                  )
                )
                (o: {
                  postInstall =
                    o.postInstall or ""
                    + ''
                      wrapProgram $out/bin/hci --prefix PATH : ${makeBinPath (lib.optional pkgs.stdenv.isLinux pkgs.runc)}
                    '';
                }
                );
              hercules-ci-cnix-expr = addNixVersionFlag
                (addBuildDepends
                  (callPkg super "hercules-ci-cnix-expr" ../hercules-ci-cnix-expr { bdw-gc = pkgs.boehmgc-hercules; inherit nix; })
                  [
                    # https://github.com/NixOS/nix/pull/4904
                    nlohmann_json
                  ])
              ;
              hercules-ci-cnix-store = addNixVersionFlag
                (addBuildDepends
                  (callPkg super "hercules-ci-cnix-store" ../hercules-ci-cnix-store { inherit nix; })
                  [
                    # https://github.com/NixOS/nix/pull/4904
                    nlohmann_json
                  ])
              ;

              websockets = updateTo "0.12.6.1" super.websockets (self.callPackage ./websockets.nix { });

              servant-websockets = self.callPackage ./servant-websockets.nix { };

            }
        );

      hercules-ci-api-swagger =
        pkgs.callPackage ../hercules-ci-api/swagger.nix { inherit (haskellPackages) hercules-ci-api; };

      tests =
        recurseIntoAttrs
          { } //
        # isx86_64: Don't run the VM tests on aarch64 to save time
        optionalAttrs (pkgs.stdenv.isLinux && pkgs.stdenv.isx86_64) {
          agent-functional-test = pkgs.nixosTest (import ../tests/agent-test.nix { daemonIsNixUnstable = false; });
          agent-functional-test-daemon-nixUnstable = pkgs.nixosTest (import ../tests/agent-test.nix { daemonIsNixUnstable = true; });
        } // optionalAttrs pkgs.stdenv.isDarwin {
          nix-darwin-example = pkgs.callPackage ../tests/nix-darwin-example.nix { };
        };

      projectRootSource = ../.;
    };
in
recurseIntoAttrs {
  inherit (internal.haskellPackages) hercules-ci-agent hercules-ci-cli;
  inherit (internal) hercules-ci-api-swagger tests;

  pre-commit-check =
    (import (pre-commit-hooks-nix + "/nix") { inherit (pkgs) system; }).run {
      src = ../.;
      tools = {
        inherit (pkgs) ormolu;
      };
      hooks = {
        # TODO: hlint.enable = true;
        ormolu.enable = true;
        ormolu.excludes = [
          # CPP
          "Hercules/Agent/Compat.hs"
          "Hercules/Agent/StoreFFI.hs"
        ];
        shellcheck.enable = true;
        nixpkgs-fmt.enable = true;
        nixpkgs-fmt.excludes = [ "tests/agent-test/testdata/" ];
      };
      settings.ormolu.defaultExtensions = [ "TypeApplications" ];
    };

  # Not traversed for derivations:
  inherit internal;
}
