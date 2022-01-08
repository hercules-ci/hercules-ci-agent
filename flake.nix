{
  description = "Hercules CI Agent";

  inputs.nixos-20_09.url = "github:NixOS/nixpkgs/nixos-20.09";
  inputs.nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.nix-darwin.url = "github:LnL7/nix-darwin"; # test only
  inputs.flake-compat.url = "github:edolstra/flake-compat";
  inputs.flake-compat.flake = false;
  inputs.pre-commit-hooks-nix.url = "github:cachix/pre-commit-hooks.nix";
  inputs.pre-commit-hooks-nix.flake = false;

  outputs =
    inputs@{ self
    , nixos-20_09
    , nixos-unstable
    , ...
    }:
    let
      lib = defaultNixpkgs.lib;
      filterMeta = defaultNixpkgs.lib.filterAttrs (k: v: k != "meta" && k != "recurseForDerivations");
      dimension = _name: attrs: f: lib.mapAttrs f attrs;

      defaultNixpkgs = nixos-unstable;
      defaultTarget = allTargets."nixos-unstable";
      testSuiteTarget = defaultTarget;

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

      allTargets =
        dimension "Nixpkgs version"
          {
            # Cachix 0.6 does not support GHC < 8.10
            # "nixos-20_09" = {
            #   nixpkgsSource = nixos-20_09;
            # };
            "nixos-unstable" = {
              nixpkgsSource = nixos-unstable;
              isDevVersion = true;
            };
            "nixos-unstable-nix_2_3" = {
              nixpkgsSource = nixos-unstable;
              isDevVersion = true;
              overlay = final: prev: {
                nix = addDebug prev.nix_2_3;
              };
            };
            "nixos-unstable-nixUnstable" = {
              nixpkgsSource = nixos-unstable;
              isDevVersion = true;
              overlay = final: prev: {
                nix = addDebug prev.nixUnstable;
              };
            };
          }
          (
            _name: { nixpkgsSource, isDevVersion ? false, overlay ? (_: _: { }) }:
              dimension "System"
                {
                  "aarch64-darwin" = {
                    # eval error (FIXME)
                    isDevSystem = false;
                  };
                  "aarch64-linux" = {
                    # shellcheck was broken https://hercules-ci.com/github/hercules-ci/hercules-ci-agent/jobs/826
                    isDevSystem = false;
                  };
                  "x86_64-linux" = { };
                  "x86_64-darwin" = { };
                }
                (system: { isDevSystem ? true }:
                  let
                    pkgs =
                      import nixpkgsSource {
                        overlays = [ (import ./nix/make-overlay.nix inputs) dev-and-test-overlay ]
                          ++ [
                          overlay
                        ];
                        config = { };
                        inherit system;
                      };
                    dev-and-test-overlay =
                      self: pkgs:
                      {
                        testSuitePkgs = testSuiteTarget.${system}.internal.pkgs;
                        devTools =
                          {
                            inherit (self.hercules-ci-agent-packages.internal.haskellPackages)
                              ghc
                              ghcid
                              stack
                              ;
                            inherit (pkgs)
                              jq
                              cabal2nix
                              nix-prefetch-git
                              niv
                              valgrind
                              ;
                            inherit pkgs;
                          };
                      };
                  in
                  pkgs.recurseIntoAttrs
                    {
                      internal.pkgs = pkgs;
                      internal.haskellPackages = pkgs.hercules-ci-agent-packages.internal.haskellPackages;
                      inherit (pkgs.hercules-ci-agent-packages)
                        hercules-ci-cli
                        hercules-ci-api-swagger
                        tests
                        ;
                      inherit (pkgs)
                        hercules-ci-agent
                        ;
                    } // lib.optionalAttrs (isDevSystem && isDevVersion) {
                    inherit (pkgs)
                      pre-commit-check
                      devTools
                      ;
                  }
                )
          );

      flakeModule = { config, lib, options, pkgs, ... }: {
        _file = "${toString ./flake.nix}##flakeModule";
        config =
          let
            mkIfNotNull = x: lib.mkIf (x != null) x;
          in
          {
            services.hercules-ci-agent.package = self.packages.${pkgs.system}.hercules-ci-agent; # defaultPriority below
            services.hercules-ci-agent.settings.labels.agent.source = "flake";
            services.hercules-ci-agent.settings.labels.agent.revision =
              mkIfNotNull (
                if (self?rev
                  && options.services.hercules-ci-agent.package.highestPrio == lib.modules.defaultPriority
                )
                then self.rev
                else if config.services.hercules-ci-agent.package ? rev
                then config.services.hercules-ci-agent.package.rev
                else null
              );
          };
      };

    in
    {
      # non-standard attribute
      ciChecks = lib.mapAttrs (k: v: v // { recurseForDerivations = true; }) allTargets;

      internal.pkgs = lib.mapAttrs (_sys: target: target.internal.pkgs) defaultTarget;

      packages =
        defaultNixpkgs.lib.mapAttrs
          (
            system: v:
              {
                inherit (v)
                  hercules-ci-agent
                  hercules-ci-cli
                  ;

                hercules-ci-agent-nixUnstable =
                  allTargets."nixos-unstable-nixUnstable".${system}.hercules-ci-agent;
                hercules-ci-cli-nixUnstable =
                  allTargets."nixos-unstable-nixUnstable".${system}.hercules-ci-cli;

                hercules-ci-agent-nix_2_3 =
                  allTargets."nixos-unstable-nix_2_3".${system}.hercules-ci-agent;
                hercules-ci-cli-nix_2_3 =
                  allTargets."nixos-unstable-nix_2_3".${system}.hercules-ci-cli;

                hercules-ci-agent-nix_2_4 = v.hercules-ci-agent;
                hercules-ci-cli-nix_2_4 = v.hercules-ci-cli;
              }
          )
          defaultTarget;

      overlay =
        final: prev: (import ./nix/make-overlay.nix inputs) final prev;

      # A module like the one in Nixpkgs
      nixosModules.agent-service =
        { pkgs, ... }:
        {
          _file = "${toString ./flake.nix}#nixosModules.agent-service";
          imports = [
            flakeModule
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
            flakeModule
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
      # Ongoing support for this module is not guaranteed, as this architecture
      # is not optimal for managing parallelism.
      nixosModules.stop-gap-multi-agent-service =
        { pkgs, ... }:
        {
          _file = "${toString ./flake.nix}#nixosModules.stop-gap-multi-agent-service";
          imports = [
            flakeModule
            ./internal/nix/nixos/multi.nix
          ];

          # Existence of the original module could cause confusion, even if they
          # can technically coexist.
          disabledModules = [ "services/continuous-integration/hercules-ci-agent/default.nix" ];

          config = {
            services.hercules-ci-agent.settings.labels.module = "nixos-multi-service";
          };
        };

      # A nix-darwin module
      darwinModules.agent-service =
        { pkgs, ... }:
        {
          _file = "${toString ./flake.nix}#darwinModules.agent-service";
          imports = [
            flakeModule
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
            flakeModule
            ./internal/nix/nix-darwin/default.nix
            ./internal/nix/gc.nix
          ];

          # This module replaces what's provided by nix-darwin
          disabledModules = [ "services/hercules-ci-agent" ];

          config = {
            services.hercules-ci-agent.settings.labels.module = "darwin-profile";
          };
        };

      defaultApp = lib.mapAttrs (k: v: v.hercules-ci-cli) self.packages;

      defaultTemplate = self.templates.nixos;
      templates = {
        nixos = {
          path = ./templates/nixos;
          description = "A NixOS configuration with Hercules CI Agent";
        };
      };

      devShell = lib.mapAttrs
        (
          system: { internal, devTools, pre-commit-check, ... }:
            internal.pkgs.mkShell {
              NIX_PATH = "nixpkgs=${internal.pkgs.path}";
              nativeBuildInputs =
                [
                  (internal.pkgs.writeScriptBin "stack" ''
                    #!/bin/sh
                    export PATH="${internal.haskellPackages.stack}/bin:$PATH"
                    if test -n "''${HIE_BIOS_OUTPUT:-}"; then
                        echo | stack --test "$@"

                        # # Internal packages appear in -package flags for some
                        # # reason, unlike normal packages. This filters them out.
                        # sed -e 's/^-package=z-.*-z-.*$//' \
                        #     -e 's/^-package-id=hercules-ci-agent.*$//' \
                        #     -i $HIE_BIOS_OUTPUT

                        # # To support the CPP in Hercules.Agent.StoreFFI
                        # echo '-DGHCIDE=1' >>$HIE_BIOS_OUTPUT

                        # # Hack to include the correct snapshot directory
                        # echo "-package-db=$(dirname $(stack path --snapshot-doc-root))/pkgdb" >> $HIE_BIOS_OUTPUT
                    else
                        exec stack "$@"
                    fi
                  '')
                  devTools.ghcid
                  devTools.jq
                  devTools.cabal2nix
                  devTools.nix-prefetch-git
                  devTools.valgrind
                  internal.haskellPackages.ghc
                  internal.haskellPackages.ghcide
                  internal.haskellPackages.haskell-language-server
                ];
              shellHook = ''
                ${pre-commit-check.shellHook}
                export STACK_ROOT=~/.stack-hercules-ci-agent
              '';
            }
        )
        defaultTarget;
    };
}
