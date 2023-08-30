{
  description = "Hercules CI Agent";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
  inputs.haskell-flake.url = "github:srid/haskell-flake/0.3.0";

  # Optional. Omit to use nixpkgs' nix
  # inputs.nix = {
  #   url = "github:NixOS/nix/master";
  #   inputs.nixpkgs.follows = "nixpkgs";
  # };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    let
      inherit (nixpkgs.legacyPackages.x86_64-linux) emptyFile;
      debug = false;
      ifDebug = f:
        if debug then f else x: x;
      addDebug = ifDebug (pkg:
        pkg.overrideAttrs (o: {
          dontStrip = true;
          enableDebugging = true;
          separateDebugInfo = false;
        })
      );

      agentFromFlakeConfig = cfg: opts: pkgs: lib:
        let
          mkIfNotNull = x: lib.mkIf (x != null) x;
        in
        {
          package = self.packages.${pkgs.system}.hercules-ci-agent; # defaultPriority below
          settings.labels.agent.source = "flake";
          settings.labels.agent.revision =
            mkIfNotNull (
              if (self?rev
                && opts.package.highestPrio == lib.modules.defaultOverridePriority or lib.modules.defaultPriority
              )
              then self.rev
              else if cfg.package ? rev
              then cfg.package.rev
              else null
            );
        };

      agentFromFlakeModule = { config, lib, options, pkgs, ... }: {
        _file = "${toString ./flake.nix}##flakeModule";
        config.services.hercules-ci-agent =
          agentFromFlakeConfig
            config.services.hercules-ci-agent
            options.services.hercules-ci-agent
            pkgs
            lib;
      };

      agentFromFlakeModule_multi = { config, lib, options, pkgs, ... }: {
        _file = "${toString ./flake.nix}##flakeModule_multi";
        options =
          let
            mkIfNotNull = x: lib.mkIf (x != null) x;
            inherit (lib) types mkOption;
          in
          {
            services.hercules-ci-agents =
              mkOption {
                type = types.attrsOf (
                  types.submoduleWith {
                    modules = [
                      ({ options, config, ... }: {
                        config = agentFromFlakeConfig config options pkgs lib;
                      })
                    ];
                  }
                );
              };
          };
      };


      suffixAttrs = suf: inputs.nixpkgs.lib.mapAttrs' (n: v: { name = n + suf; value = v; });
    in
    flake-parts.lib.mkFlake { inherit inputs; } (flakeArgs@{ config, lib, inputs, ... }: {
      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
        inputs.haskell-flake.flakeModule
        ./nix/variants.nix
        ./nix/flake-private-dev-inputs.nix
      ];
      config = {
        privateDevInputSubflakePath = "dev/private";
        partitionedAttrs.checks = "dev";
        partitionedAttrs.devShells = "dev";
        partitionedAttrs.herculesCI = "dev";
        partitions.dev.settings = { inputs, ... }: {
          imports = [
            ./nix/development.nix
            ./nix/flake-update-pre-commit.nix
            inputs.hercules-ci-effects.flakeModule
            inputs.pre-commit-hooks-nix.flakeModule
          ];
        };
        systems = [
          "aarch64-darwin"
          "aarch64-linux"
          "x86_64-darwin"
          "x86_64-linux"
        ];
        flake = {
          overlay = config.flake.overlays.default;

          # A module like the one in Nixpkgs
          nixosModules.agent-service =
            { lib, pkgs, ... }:
            {
              _file = "${toString ./flake.nix}#nixosModules.agent-service";
              imports = [
                agentFromFlakeModule
                ./internal/nix/nixos/default.nix
              ];

              # This module replaces what's provided by NixOS
              disabledModules = [ "services/continuous-integration/hercules-ci-agent/default.nix" ];

              config = {
                services.hercules-ci-agent.settings.labels.module = "nixos-service";
              };
            };

          # An opinionated module for configuring an agent machine
          nixosModules.agent-profile =
            { lib, pkgs, ... }:
            {
              _file = "${toString ./flake.nix}#nixosModules.agent-profile";
              imports = [
                agentFromFlakeModule
                ./internal/nix/nixos/default.nix
                ./internal/nix/deploy-keys.nix
                ./internal/nix/gc.nix
              ];

              # This module replaces what's provided by NixOS
              disabledModules = [ "services/continuous-integration/hercules-ci-agent/default.nix" ];

              config = {
                services.hercules-ci-agent.settings.labels.module = "nixos-profile";
              };
            };

          # A module for configuring multiple agents on a single machine
          nixosModules.multi-agent-service =
            { lib, pkgs, ... }:
            {
              _file = "${toString ./flake.nix}#nixosModules.multi-agent-service";
              imports = [
                agentFromFlakeModule_multi
                ./internal/nix/nixos/multi.nix
              ];

              # Existence of the original module could cause confusion, even if they
              # can technically coexist.
              disabledModules = [ "services/continuous-integration/hercules-ci-agent/default.nix" ];

              options = let inherit (lib) types mkOption; in
                {
                  services.hercules-ci-agents =
                    mkOption {
                      type = types.attrsOf (
                        types.submoduleWith {
                          modules = [{ config.settings.labels.module = "nixos-multi-service"; }];
                        }
                      );
                    };
                };
            };

          # A nix-darwin module
          darwinModules.agent-service =
            { lib, pkgs, ... }:
            {
              _file = "${toString ./flake.nix}#darwinModules.agent-service";
              imports = [
                agentFromFlakeModule
                ./internal/nix/nix-darwin/default.nix
              ];

              # This module replaces what's provided by nix-darwin
              disabledModules = [ "services/hercules-ci-agent" ];

              config = {
                services.hercules-ci-agent.settings.labels.module = "darwin-service";
              };
            };

          # A nix-darwin module with more defaults set for machines that serve as agents
          darwinModules.agent-profile =
            { lib, pkgs, ... }:
            {
              _file = "${toString ./flake.nix}#darwinModules.agent-profile";
              imports = [
                agentFromFlakeModule
                ./internal/nix/nix-darwin/default.nix
                ./internal/nix/gc.nix
              ];

              # This module replaces what's provided by nix-darwin
              disabledModules = [ "services/hercules-ci-agent" ];

              config = {
                services.hercules-ci-agent.settings.labels.module = "darwin-profile";
              };
            };

          defaultApp = lib.mapAttrs (k: v: { program = v.hercules-ci-cli + "/bin/hci"; type = "app"; }) self.packages;

          defaultTemplate = self.templates.nixos;
          templates = {
            nixos = {
              path = ./templates/nixos;
              description = "A NixOS configuration with Hercules CI Agent";
            };
          };
        };
        perSystem = { config, pkgs, system, ... }:
          let
            dev-and-test-overlay = self: pkgs:
              {
                testSuitePkgs = pkgs; # TODO: reuse pkgs via self so we don't build a variant
                nix =
                  if inputs?nix
                  then
                    inputs.nix.packages.${pkgs.stdenv.hostPlatform.system}.default or (
                      lib.warn
                        "The `nix` flake does not define a package for system ${pkgs.stdenv.hostPlatform.system}. Using nixpkgs' `nix` instead."
                        pkgs.nix
                    )
                  else
                    pkgs.nix;
              };

            h = pkgs.haskell.lib.compose;

            addCompactUnwind =
              if pkgs.stdenv.hostPlatform.isDarwin
              then h.appendConfigureFlags [ "--ghc-option=-fcompact-unwind" ]
              else x: x;

            nix = pkgs.nix;

          in
          {
            config = {
              # public overlay
              overlayAttrs = {
                inherit (config.packages) hercules-ci-cli;
                hercules-ci-agent = config.packages.hercules-ci-agent // lib.optionalAttrs (self.sourceInfo?rev) { rev = self.sourceInfo.rev; };
                hci = config.packages.hercules-ci-cli;
              };

              # internal pkgs
              _module.args.pkgs =
                import config.nixpkgsSource {
                  overlays = [
                    dev-and-test-overlay
                    flakeArgs.config.extraOverlay
                  ];
                  config = { };
                  inherit system;
                };

              apps.hci = config.apps.internal-hci;
              apps.hercules-ci-agent = config.apps.internal-hercules-ci-agent;

              haskellProjects = {
                internal = {

                  devShell.extraLibraries = hp: {
                    inherit (hp) releaser;
                    # Cachix deps (cachix was excluded because of its dependency on cnix-store)
                  } //
                  lib.listToAttrs (
                    map
                      (p: lib.nameValuePair p.pname p)
                      (lib.filter
                        (p: p?pname && p.pname != "hercules-ci-cnix-store")
                        hp.cachix_saved.getCabalDeps.libraryHaskellDepends
                      )
                  );

                  overrides = self: super: {

                    cachix_saved = super.cachix;

                    cachix =
                      if flakeArgs.config.isForDevShell
                      then
                      # For the shell we omit cachix from the dependency search
                      # so as not to introduce a nix-built dependency on cnix-store
                        null
                      else if builtins.compareVersions super.cachix.version "1.5" >= 0
                      then
                        super.cachix.override
                          (o: {
                            inherit nix;
                          })
                      else
                        ((super.cachix_1_3_3 or super.cachix).override (o: {
                          inherit nix;
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
                      super.callPackage ./nix/hercules-ci-optparse-applicative.nix { };

                    inline-c-cpp =
                      # https://github.com/fpco/inline-c/pull/132
                      assert lib.any
                        (patch: lib.hasSuffix "inline-c-cpp-pr-132-1.patch" (baseNameOf patch))
                        super.inline-c-cpp.patches;
                      super.inline-c-cpp;

                    hercules-ci-agent = lib.pipe super.hercules-ci-agent [
                      h.justStaticExecutables
                      (h.addBuildTool pkgs.makeBinaryWrapper)
                      addCompactUnwind
                      h.enableDWARFDebugging
                      (h.addBuildDepends [ pkgs.boost ])
                      (h.overrideCabal (o: {
                        postCompileBuildDriver = ''
                          echo Setup version:
                          ./Setup --version
                        '';
                        postInstall = ''
                          ${o.postInstall or ""}
                          mkdir -p $out/libexec
                          mv $out/bin/hercules-ci-agent $out/libexec
                          makeWrapper $out/libexec/hercules-ci-agent $out/bin/hercules-ci-agent --prefix PATH : ${lib.makeBinPath 
                            ([ pkgs.gnutar pkgs.gzip pkgs.git pkgs.openssh ]
                             ++ lib.optional pkgs.stdenv.isLinux pkgs.runc)}
                        '';
                        passthru = o.passthru or { } // {
                          inherit nix;
                        };
                      }))
                      (self.generateOptparseApplicativeCompletions [ "hercules-ci-agent" ])
                    ];

                    hercules-ci-agent_lib = lib.pipe self.hercules-ci-agent
                      # Don't override this when making a devShell, because Nixpkgs shellFor does not recognize it as a local dependency.
                      (lib.optionals (!flakeArgs.config.isForDevShell) [
                        (h.overrideCabal (o: {
                          isLibrary = true;
                          isExecutable = false;
                          postFixup = "";
                        }))
                      ])
                    ;
                    hercules-ci-cli = lib.pipe super.hercules-ci-cli [
                      (x: x.override (o: {
                        hercules-ci-agent = self.hercules-ci-agent_lib;
                      }))
                      (h.addBuildTool pkgs.makeBinaryWrapper)
                      addCompactUnwind
                      h.disableLibraryProfiling
                      h.justStaticExecutables
                      (self.generateOptparseApplicativeCompletions [ "hci" ])
                      (h.overrideCabal (o:
                        let binPath = lib.optionals pkgs.stdenv.isLinux [ pkgs.runc ];
                        in
                        {
                          postInstall =
                            o.postInstall or ""
                            + lib.optionalString (binPath != [ ]) ''
                              wrapProgram $out/bin/hci --prefix PATH : ${lib.makeBinPath binPath}
                            '';
                        }
                      ))
                    ];

                    # FIXME: https://github.com/hercules-ci/hercules-ci-agent/pull/443/files
                    hercules-ci-cnix-expr = lib.pipe super.hercules-ci-cnix-expr [
                      (x: x.override (o: { inherit nix; }))
                      (h.addBuildTool pkgs.git)
                    ];

                    hercules-ci-cnix-store = lib.pipe super.hercules-ci-cnix-store [
                      (x: x.override (o: { inherit nix; }))
                      (x: x.overrideAttrs (o: {
                        passthru = o.passthru // { nixPackage = nix; };
                      }))
                    ];

                    # Permission denied error in tests. Might be a system configuration error on the machine?
                    # TODO: see if rio builds on hydra.nixos.org after https://github.com/NixOS/nixpkgs/pull/160733
                    rio = h.dontCheck super.rio;

                    hie-bios = h.appendPatch ./nix/hie-bios.patch super.hie-bios;

                    # Dodge build failures of components we don't need.
                    haskell-language-server = h.appendConfigureFlags [ "-f-fourmolu" ] (
                      super.haskell-language-server.override {
                        hls-fourmolu-plugin = null;
                      }
                    );

                    ghcid = (
                      if system == "aarch64-darwin"
                      then h.overrideCabal (drv: { enableSeparateBinOutput = false; })
                      else x: x
                    ) super.ghcid;
                    ormolu = (
                      if system == "aarch64-darwin"
                      then h.overrideCabal (drv: { enableSeparateBinOutput = false; })
                      else x: x
                    ) super.ormolu;

                  };
                };
              };
              packages.hercules-ci-api-swagger =
                pkgs.callPackage ./hercules-ci-api/swagger.nix { hercules-ci-api = config.packages.internal-hercules-ci-api; };
              packages.hercules-ci-cli = config.packages.internal-hercules-ci-cli;
              packages.hercules-ci-agent = config.packages.internal-hercules-ci-agent;

              # packages.hercules-ci-agent-nixUnstable = config.variants.nixUnstable.packages.hercules-ci-agent;
              # packages.hercules-ci-cli-nixUnstable = config.variants.nixUnstable.packages.hercules-ci-cli;
              devShells.default =
                let
                  # TODO haskell-flake final packages
                  inherit (pkgs) haskellPackages;
                  shellWithHaskell = true;
                  baseShell =
                    if shellWithHaskell
                    then config.devShells.internal
                    else pkgs.mkShell { };
                  shell = baseShell.overrideAttrs (o: {
                    NIX_PATH = "nixpkgs=${pkgs.path}";
                    NIXPKGSBALL = pkgs.callPackage ./tests/nixpkgsball.nix { };
                    nativeBuildInputs =
                      o.nativeBuildInputs or [ ] ++ [
                        pkgs.jq
                        pkgs.nix-prefetch-git
                        pkgs.nixpkgs-fmt
                        # pkgs.haskell.packages.ghc8107.stack
                        pkgs.haskellPackages.stack
                        pkgs.pre-commit
                        # pkgs.valgrind (broken on x86_64-darwin)
                      ] ++ lib.optionals shellWithHaskell [
                        haskellPackages.haskell-language-server
                        pkgs.haskellPackages.implicit-hie # gen-hie
                        pkgs.haskellPackages.ghcid
                      ];
                    shellHook = ''
                      ${o.shellHook or ""}
                      if [[ -z "''${IN_LORRI_SHELL:-}" ]]; then
                      ${config.pre-commit.installationScript}
                      fi
                    '';
                  });
                in
                shell;

              checks = config.checkSet
                # // suffixAttrs "-nixUnstable" config.variants.nixUnstable.checkSet
              ;

              checkSet =
                let
                  multi-example = pkgs.callPackage ./tests/multi-example.nix { };
                in
                {
                  cli = pkgs.callPackage ./tests/cli.nix { hci = config.packages.hercules-ci-cli; };
                }
                # isx86_64: Don't run the VM tests on aarch64 to save time
                // lib.optionalAttrs (pkgs.stdenv.isLinux && pkgs.stdenv.isx86_64)
                  {
                    agent-functional-test = pkgs.nixosTest (import ./tests/agent-test.nix { flake = self; daemonIsNixUnstable = false; trusted = true; });
                    agent-functional-test-untrusted = pkgs.nixosTest (import ./tests/agent-test.nix { flake = self; daemonIsNixUnstable = false; trusted = false; });
                    # agent-functional-test-daemon-nixUnstable = pkgs.nixosTest (import ./tests/agent-test.nix { flake = self; daemonIsNixUnstable = true; });
                    multi-example-eq = multi-example.eq;
                    multi-example-multi = multi-example.multi;
                  } // lib.optionalAttrs pkgs.stdenv.isDarwin {
                  nix-darwin-example =
                    pkgs.callPackage ./tests/nix-darwin-example.nix { flake = self // { inherit inputs; }; };
                }
                // lib.optionalAttrs (system == "x86_64-linux") {
                  evalTests =
                    (import ./tests/default-herculesCI-for-flake-test.nix
                      { inherit (nixpkgs) lib; }).test
                      pkgs.emptyFile;
                };
            };
            options = {
              nixpkgsSource = lib.mkOption {
                default = inputs.nixpkgs;
              };
              checkSet = lib.mkOption {
                description = "All tests, excluding those from variants.";
              };
            };
          };
        # variants.nixUnstable.extraOverlay = final: prev: {
        #   nix = addDebug inputs.nix.defaultPackage.${prev.stdenv.hostPlatform.system};
        # };

        variants.forDevShell.isForDevShell = true;
        # Take the devShells from the dev variant
        flake.devShells = lib.mkIf (!config.isForDevShell) (
          lib.mkOverride ((lib.mkForce { }).priority - 1) (
            config.variants.forDevShell.flake.devShells
          )
        );
      };
      options = {
        # Set by variants
        extraOverlay = lib.mkOption {
          default = _: _: { };
        };
        isForDevShell = lib.mkOption {
          default = false;
          description = ''
            Whether we're producing a development attribute.

            We apply some overrides to fix things up in the context of devShell.
          '';
        };
      };
    });
}
