{ haskellPackages
, haskell
, pkgs
, nix
  # TODO propagatedBuildInputs upstream
, nlohmann_json
, pre-commit-hooks-nix
, flake
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
    appendConfigureFlags
    appendPatch
    disableLibraryProfiling
    enableDWARFDebugging
    doJailbreak
    generateOptparseApplicativeCompletion
    justStaticExecutables
    overrideCabal
    overrideSrc
    ;

  updateTo = v: stdPkg: altPkg:
    if lib.versionAtLeast stdPkg.version v
    then stdPkg
    else altPkg;

  addCompactUnwind =
    if pkgs.stdenv.hostPlatform.isDarwin
    then x: appendConfigureFlags x [ "--ghc-option=-fcompact-unwind" ]
    else x: x;

  internal =
    rec {
      inherit pkgs;

      callPkg = super: name: srcPath: args: haskellPackages.buildFromCabalSdist (super.callCabal2nix name srcPath args);
      callPkgOpts = opts: super: name: srcPath: args: haskellPackages.buildFromCabalSdist (super.callCabal2nixWithOptions name srcPath opts args);

      # TODO: upstream the overrides
      haskellPackages =
        haskellPackages_.extend (
          self: super:
            {
              cabal-pkg-config-version-hook =
                callPkg super "cabal-pkg-config-version-hook" ../cabal-pkg-config-version-hook { };

              # Must match hercules-ci-cnix-store, which uses `pkgs.nix`.
              # Nixpkgs may override to a specific series.
              cachix = (super.cachix.override (o: {
                nix = pkgs.nix;
              })).overrideAttrs (o: {
                postPatch = ''
                  ${o.postPatch or ""}
                  # jailbreak pkgconfig deps
                  cp cachix.cabal cachix.cabal.backup
                  sed -i cachix.cabal -e 's/\(nix-[a-z]*\) *(==[0-9.]* *|| *>[0-9.]*) *&& *<[0-9.]*/\1/g'
                  sed -i cachix.cabal -e 's/pkgconfig-depends:.*/pkgconfig-depends: nix-main, nix-store/'
                  echo
                  echo Applied:
                  diff -U5 cachix.cabal.backup cachix.cabal ||:
                  echo
                  rm cachix.cabal.backup
                '';
              });

              hercules-ci-optparse-applicative =
                super.callPackage ./hercules-ci-optparse-applicative.nix { };
              inline-c-cpp =
                # https://github.com/fpco/inline-c/pull/132
                assert lib.any (patch: lib.hasSuffix "inline-c-cpp-pr-132-1.patch" (baseNameOf patch)) super.inline-c-cpp.patches;
                super.inline-c-cpp;

              hercules-ci-api = callPkg super "hercules-ci-api" ../hercules-ci-api { };
              hercules-ci-api-agent = callPkg super "hercules-ci-api-agent" ../hercules-ci-api-agent { };
              hercules-ci-api-core = callPkg super "hercules-ci-api-core" ../hercules-ci-api-core { };

              hercules-ci-agent =
                let
                  basePkg =
                    callPkg super "hercules-ci-agent" ../hercules-ci-agent { };
                  bundledBins = [ pkgs.gnutar pkgs.gzip pkgs.git pkgs.openssh ] ++ lib.optional pkgs.stdenv.isLinux pkgs.runc;

                in
                generateOptparseApplicativeCompletion "hercules-ci-agent" (
                  overrideCabal
                    (
                      addBuildDepends
                        (enableDWARFDebugging
                          (addCompactUnwind
                            (addBuildTool (justStaticExecutables basePkg)
                              pkgs.makeBinaryWrapper
                            )
                          )
                        )
                        [ pkgs.boost ]
                    )
                    (
                      o:
                      {
                        postCompileBuildDriver = ''
                          echo Setup version:
                          ./Setup --version
                        '';

                        postInstall = ''
                          ${o.postInstall or ""}
                          mkdir -p $out/libexec
                          mv $out/bin/hercules-ci-agent $out/libexec
                          makeWrapper $out/libexec/hercules-ci-agent $out/bin/hercules-ci-agent --prefix PATH : ${makeBinPath bundledBins}
                        '';
                        passthru =
                          (o.passthru or { })
                          // {
                            inherit nix;
                          };
                      }
                    )
                );

              hercules-ci-agent-test =
                callPkg super "hercules-ci-agent-test" ../tests/agent-test { };

              hercules-ci-agent_lib = overrideCabal self.hercules-ci-agent (o: {
                isLibrary = true;
                isExecutable = false;
                postFixup = "";
              });

              hercules-ci-cli = overrideCabal
                (
                  generateOptparseApplicativeCompletion "hci" (
                    justStaticExecutables (
                      haskell.lib.disableLibraryProfiling (
                        addCompactUnwind (
                          haskell.lib.compose.addBuildTool pkgs.makeBinaryWrapper (
                            callPkg super "hercules-ci-cli" ../hercules-ci-cli {
                              hercules-ci-agent = self.hercules-ci-agent_lib;
                            }
                          )
                        )
                      )
                    )
                  )
                )
                (o:
                  let binPath = lib.optionals pkgs.stdenv.isLinux [ pkgs.runc ];
                  in
                  {
                    postInstall =
                      o.postInstall or ""
                      + lib.optionalString (binPath != [ ]) ''
                        wrapProgram $out/bin/hci --prefix PATH : ${makeBinPath binPath}
                      '';
                  }
                );
              hercules-ci-cnix-expr =
                addBuildTool
                  (callPkgOpts "--extra-arguments nix-cmd" super "hercules-ci-cnix-expr" ../hercules-ci-cnix-expr {
                    inherit nix;
                    # https://github.com/NixOS/cabal2nix/pull/546
                    nix-cmd = nix;
                  })
                  pkgs.git;
              hercules-ci-cnix-store =
                callPkg super "hercules-ci-cnix-store" ../hercules-ci-cnix-store { inherit nix; };

              # Permission denied error in tests. Might be a system configuration error on the machine?
              # TODO: see if rio builds on hydra.nixos.org after https://github.com/NixOS/nixpkgs/pull/160733
              rio = haskell.lib.dontCheck super.rio;

              hie-bios = haskell.lib.compose.appendPatch ./hie-bios.patch super.hie-bios;

              # Dodge build failures of components we don't need.
              haskell-language-server = haskell.lib.compose.appendConfigureFlags [ "-f-fourmolu" ] (
                super.haskell-language-server.override {
                  hls-fourmolu-plugin = null;
                }
              );
            }
        );

      hercules-ci-api-swagger =
        pkgs.callPackage ../hercules-ci-api/swagger.nix { inherit (haskellPackages) hercules-ci-api; };
    };
in
recurseIntoAttrs {
  inherit (internal.haskellPackages) hercules-ci-agent hercules-ci-cli;
  inherit (internal) hercules-ci-api-swagger tests;

  # Not traversed for derivations:
  inherit internal;
}
