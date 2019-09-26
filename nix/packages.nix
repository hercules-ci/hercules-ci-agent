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

              hercules-ci-api =
                buildFromSdist (
                  overrideSrc (self.callPackage ../hercules-ci-api/pkg.nix {}) { src = gitignoreSource ../hercules-ci-api; }
                );

              hercules-ci-agent =
                let
                  basePkg =
                    overrideSrc (
                      self.callPackage ../hercules-ci-agent/pkg.nix {
                        nix-store = nix;
                        nix-expr = nix;
                        nix-main = nix;
                        bdw-gc = pkgs.boehmgc-hercules;
                      }
                    ) { src = gitignoreSource ../hercules-ci-agent; };

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
                              wrapProgram $out/bin/hercules-ci-agent --prefix PATH : ${makeBinPath [ pkgs.gnutar pkgs.gzip nix ]}
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
                buildFromSdist (
                  overrideSrc (self.callPackage ../tests/agent-test/pkg.nix {}) { src = gitignoreSource ../tests/agent-test; }
                );

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
  tests = if pkgs.stdenv.isLinux then internal.tests else null;
  pre-commit-check =
    (import sources.nix-pre-commit-hooks).run { src = gitignoreSource ../.; };

  # Not traversed for derivations:
  inherit internal;
}
