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
  inherit (haskell.lib) overrideSrc addBuildDepends overrideCabal buildFromSdist doJailbreak disableLibraryProfiling addBuildTool;
  inherit (import sources.gitignore { inherit lib; }) gitignoreSource;
  callPkg = super: name: srcPath: args: overrideSrc (super.callPackage (srcPath + "/pkg.nix") args) { src = gitignoreSource srcPath; };

  sources = import ./sources.nix;

  internal =
    rec {
      inherit pkgs;

      # TODO: upstream the overrides
      haskellPackages =
        haskellPackages_.extend (
          self: super:
            {
              cachix =
                # avoid https://gitlab.haskell.org/ghc/ghc/issues/16477
                haskell.lib.disableLibraryProfiling (
                  self.callPackage ./cachix.nix { nix-main = nix; nix-store = nix; boost_context = pkgs.boost; }
                );
              cachix-api = self.callPackage ./cachix-api.nix {};

              hercules-ci-api = callPkg super "hercules-ci-api" ../hercules-ci-api {};
              hercules-ci-api-agent = callPkg super "hercules-ci-api-agent" ../hercules-ci-api-agent {};
              hercules-ci-api-core = callPkg super "hercules-ci-api-core" ../hercules-ci-api-core {};

              hercules-ci-agent =
                let
                  basePkg =
                    callPkg super "hercules-ci-agent" ../hercules-ci-agent {
                      nix-store = nix;
                      nix-expr = nix;
                      nix-main = nix;
                      bdw-gc = pkgs.boehmgc-hercules;
                      boost_context = pkgs.boost;
                    };

                in
                  buildFromSdist (
                    overrideCabal (
                      addBuildDepends basePkg [ pkgs.makeWrapper pkgs.boost pkgs.boehmgc ]
                    ) (
                      o:
                        {
                          postInstall =
                            o.postInstall or ""
                            + ''
                              wrapProgram $out/bin/hercules-ci-agent --prefix PATH : ${makeBinPath [ pkgs.gnutar pkgs.gzip pkgs.git nix ]}
                              # TODO: worker should inherit from the agent, but can't find git without this
                              wrapProgram $out/bin/hercules-ci-agent-worker --prefix PATH : ${makeBinPath [ pkgs.gnutar pkgs.gzip pkgs.git nix ]}
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

              tomland =
                self.callPackage ./haskell-tomland-1-0-1-0.nix {
                  hedgehog = self.hedgehog_1_0;
                  tasty-hedgehog = self.tasty-hedgehog_1_0;
                };

              hedgehog_1_0 =
                self.callPackage ./haskell-hedgehog-1-0.nix {};
              tasty-hedgehog_1_0 =
                self.callPackage ./haskell-tasty-hedgehog-1-0.nix { hedgehog = self.hedgehog_1_0; };

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

              hnix-store-core = self.callPackage ./haskell-hnix-store-core.nix {};

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
