{
  description = "Hercules CI Agent";

  inputs.nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.nix-darwin.url = "github:LnL7/nix-darwin"; # test only
  inputs.pre-commit-hooks-nix.url = "github:hercules-ci/pre-commit-hooks.nix/flakeModule";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.flake-parts.inputs.nixpkgs.follows = "nixos-unstable";

  outputs = inputs@{ self, nixos-unstable, flake-parts, ... }:
    let
      inherit (nixos-unstable.legacyPackages.x86_64-linux) emptyFile;
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
                && opts.package.highestPrio == lib.modules.defaultPriority
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


      suffixAttrs = suf: inputs.nixos-unstable.lib.mapAttrs' (n: v: { name = n + suf; value = v; });
    in
    flake-parts.lib.mkFlake { inherit self; } (flakeArgs@{ config, lib, ... }: {
      imports = [
        inputs.pre-commit-hooks-nix.flakeModule
        ./variants.nix
      ];
      config = {
        systems = [
          "aarch64-darwin"
          "aarch64-linux"
          "x86_64-darwin"
          "x86_64-linux"
        ];
        flake = {
          overlay =
            final: prev: (import ./nix/make-overlay.nix self) final prev;

          # A module like the one in Nixpkgs
          nixosModules.agent-service =
            { pkgs, ... }:
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
            { pkgs, ... }:
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
            { pkgs, ... }:
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
            { pkgs, ... }:
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
            { pkgs, ... }:
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

          herculesCI.onPush.default = {
            outputs = { ... }: {
              flake = {
                inherit (self)
                  checks
                  defaultApp
                  devShells
                  legacyPackages
                  packages
                  ;
              };
            };
          };

          # Disabled checks

          # Error: https://hercules-ci.com/accounts/github/hercules-ci/derivations/%2Fnix%2Fstore%2Fi9x1mv2m95l4y4yzsgb9qgg39m4c9ql7-python3.9-pre-commit-2.18.1.drv/log?via-job=3a8af400-acee-42a1-9fb0-0ae6af20133b
          # PR: https://github.com/NixOS/nixpkgs/pull/167879
          checks.x86_64-darwin.pre-commit = lib.mkForce emptyFile;
          devShells.x86_64-darwin.default = lib.mkForce emptyFile;
        };
        perSystem = { config, pkgs, system, ... }:
          let
            dev-and-test-overlay = self: pkgs:
              {
                testSuitePkgs = pkgs; # TODO: reuse pkgs via self so we don't build a variant
                devTools =
                  {
                    inherit (self.hercules-ci-agent-packages.internal.haskellPackages)
                      ghc
                      ghcid
                      # TODO Use wrapped pkgs.cabal2nix, currently broken on darwin
                      cabal2nix
                      ;
                    inherit (pkgs)
                      jq
                      nix-prefetch-git
                      ;
                  };
              };

            isDevVariant =
              # eval error (FIXME)
              system != "aarch64-darwin"
              &&
              # shellcheck was broken https://hercules-ci.com/github/hercules-ci/hercules-ci-agent/jobs/826
              system != "aarch64-linux"
            ;

          in
          {
            config = {
              _module.args.pkgs =
                import config.nixpkgsSource {
                  overlays = [
                    (import ./nix/make-overlay.nix self)
                    dev-and-test-overlay
                    flakeArgs.config.extraOverlay
                  ];
                  config = { };
                  inherit system;
                };
              packages.hercules-ci-api-swagger = pkgs.hercules-ci-agent-packages.hercules-ci-api-swagger;
              packages.hercules-ci-cli = pkgs.hercules-ci-agent-packages.hercules-ci-cli;
              packages.hercules-ci-agent = pkgs.hercules-ci-agent;
              # packages.hercules-ci-agent-nixUnstable = config.variants.nixUnstable.packages.hercules-ci-agent;
              # packages.hercules-ci-cli-nixUnstable = config.variants.nixUnstable.packages.hercules-ci-cli;
              packages.hercules-ci-agent-nix_2_7 = config.variants.nix_2_7.packages.hercules-ci-agent;
              packages.hercules-ci-cli-nix_2_7 = config.variants.nix_2_7.packages.hercules-ci-cli;
              pre-commit.pkgs = pkgs;
              pre-commit.settings = {
                hooks = {
                  # TODO: hlint.enable = true;
                  ormolu.enable = true;
                  ormolu.excludes = [
                    # CPP
                    "Hercules/Agent/Compat.hs"
                    "Hercules/Agent/StoreFFI.hs"
                    "Hercules/CNix/Expr.hs" # parse error in quasiquotation
                    "Hercules/CNix/Store.hs" # parse error in quasiquotation + CPP
                  ];
                  shellcheck.enable = true;
                  nixpkgs-fmt.enable = true;
                  nixpkgs-fmt.excludes = [ "tests/agent-test/testdata/" ];
                };
                excludes = [
                  ".*/vendor/.*"
                ];
                settings.ormolu.defaultExtensions = [ "TypeApplications" ];
              };
              devShells.default =
                let
                  inherit (pkgs.hercules-ci-agent-packages.internal) haskellPackages;
                  shellWithHaskell = true;
                  baseShell =
                    if shellWithHaskell
                    then import ./nix/shellFor-cabal.nix { inherit haskellPackages pkgs; }
                    else pkgs.mkShell { };
                  shell = baseShell.overrideAttrs (o: {
                    NIX_PATH = "nixpkgs=${pkgs.path}";
                    NIXPKGSBALL = pkgs.callPackage ./tests/nixpkgsball.nix { };
                    nativeBuildInputs =
                      o.nativeBuildInputs or [ ] ++ [
                        pkgs.jq
                        pkgs.devTools.cabal2nix
                        pkgs.nix-prefetch-git
                        pkgs.nixpkgs-fmt
                        # pkgs.haskell.packages.ghc8107.stack
                        pkgs.haskellPackages.stack
                        # pkgs.valgrind (broken on x86_64-darwin)
                      ] ++ lib.optionals shellWithHaskell [
                        haskellPackages.haskell-language-server
                        pkgs.haskellPackages.implicit-hie # gen-hie
                        pkgs.haskellPackages.ghcid
                      ];
                    shellHook = ''
                      ${o.shellHook or ""}
                      ${config.pre-commit.installationScript}
                    '';
                  });
                in
                if isDevVariant then shell else pkgs.mkShell { name = "unsupported-shell"; };

              checks = config.checkSet
                # // suffixAttrs "-nixUnstable" config.variants.nixUnstable.checkSet
              ;

              checkSet =
                let
                  multi-example = pkgs.callPackage ./tests/multi-example.nix { };
                in
                {
                  cli = pkgs.callPackage ./tests/cli.nix { hci = pkgs.hercules-ci-agent-packages.hercules-ci-cli; };
                }
                # isx86_64: Don't run the VM tests on aarch64 to save time
                // lib.optionalAttrs (pkgs.stdenv.isLinux && pkgs.stdenv.isx86_64)
                  {
                    agent-functional-test = pkgs.nixosTest (import ./tests/agent-test.nix { flake = self; daemonIsNixUnstable = false; });
                    # agent-functional-test-daemon-nixUnstable = pkgs.nixosTest (import ./tests/agent-test.nix { flake = self; daemonIsNixUnstable = true; });
                    multi-example-eq = multi-example.eq;
                    multi-example-multi = multi-example.multi;
                  } // lib.optionalAttrs pkgs.stdenv.isDarwin {
                  nix-darwin-example = pkgs.callPackage ./tests/nix-darwin-example.nix { flake = self; };
                }
                // lib.optionalAttrs isDevVariant pkgs.devTools
                # only check pre-commit on development capable systems
                // lib.optionalAttrs (!isDevVariant) { pre-commit = lib.mkForce pkgs.emptyFile; };
            };
            options = {
              nixpkgsSource = lib.mkOption {
                default = inputs.nixos-unstable;
              };
              checkSet = lib.mkOption {
                description = "All tests, excluding those from variants.";
              };
            };
          };
        # variants.nixUnstable.extraOverlay = final: prev: {
        #   nix = addDebug inputs.nix.defaultPackage.${prev.stdenv.hostPlatform.system};
        # };
        variants.nix_2_7.extraOverlay = final: prev: {
          nix = addDebug prev.nixVersions.nix_2_7;
        };
      };
      options = {
        # Set by variants
        extraOverlay = lib.mkOption {
          default = _: _: { };
        };
      };
    });
}
