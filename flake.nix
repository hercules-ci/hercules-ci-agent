{
  description = "Hercules CI Agent";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.03";
  inputs.flake-compat.url = "github:edolstra/flake-compat";
  inputs.flake-compat.flake = false;
  inputs.project-nix.url = "github:hercules-ci/project.nix";
  inputs.project-nix.flake = false;
  inputs.pre-commit-hooks-nix.url = "github:cachix/pre-commit-hooks.nix";
  inputs.pre-commit-hooks-nix.flake = false;

  outputs = { self, nixpkgs, project-nix, ... }:
    let
      lib = nixpkgs.lib;
      filterMeta = nixpkgs.lib.filterAttrs (k: v: k != "meta" && k != "recurseForDerivations");
      inherit (import (project-nix + "/lib/dimension.nix") { inherit lib; }) dimension;

      defaultTarget = allTargets."nixos-20_03";

      allTargets =
        dimension "Nixpkgs version" {
          "nixos-20_03" = {
            nixpkgsSource = nixpkgs;
          };
          # "nixos-unstable" = {
          #   nixpkgsSource = "nixos-unstable";
          # };
        } (
          _name: { nixpkgsSource }:
            dimension "System" {
              "aarch64-linux" = { enable = true; };
              "x86_64-linux" = { enable = true; };
              "x86_64-darwin" = { enable = true; };
            } (
              system: { enable }:

                lib.optionalAttrs enable (
                  let
                    pkgs = import ./nix/default.nix {
                      nixpkgs = nixpkgsSource;
                      inherit system allTargets;
                    };
                  in
                    pkgs.recurseIntoAttrs {
                      internal.pkgs = pkgs;
                      inherit (pkgs.hercules-ci-agent-packages)
                        hercules-ci-cli
                        ;
                      inherit (pkgs)
                        hercules-ci-agent
                        hercules-ci-agent-packages
                        devTools
                        toTOML-test
                        pre-commit-check
                        ;
                    }
                )
            )
        );

    in
      {
        # non-standard attribute
        ciChecks = allTargets;

        packages =
          nixpkgs.lib.mapAttrs
            (
              k: v:
                {
                  inherit (filterMeta v)
                    hercules-ci-agent
                    hercules-ci-cli
                    ;
                }
            )
            (filterMeta defaultTarget);

        # TODO
        # nixosModules.agent-service = { imports = [ ./module.nix ]; };
        nixosModules.agent-profile = { imports = [ ./module.nix ]; };

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
            k: { internal, devTools, hercules-ci-agent-packages, ... }:
              internal.pkgs.mkShell {
                buildInputs =
                  [
                    devTools.niv
                    devTools.stack
                    devTools.ghcid
                    devTools.jq
                    devTools.cabal2nix
                    devTools.nix-prefetch-git
                    hercules-ci-agent-packages.internal.haskellPackages.ghc
                    hercules-ci-agent-packages.internal.haskellPackages.ghcide
                  ];
                inherit (hercules-ci-agent-packages.pre-commit-check) shellHook;
              }
          )
          defaultTarget;
      };
}
