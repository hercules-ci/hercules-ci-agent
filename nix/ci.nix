let
  sources = import ./sources.nix;
  defaultNixpkgsSource = "nixos-20.09";

  lib = import (sources.${defaultNixpkgsSource} + "/lib");
  inherit (import sources."project.nix" { inherit lib; }) dimension;

  # nix-build doesn't traverse names with periods...
  allTargets = dimension "Nixpkgs version" {
    "nixos-20_09" = {
      nixpkgsSource = "nixos-20.09";
    };
    "nixos-unstable" = {
      nixpkgsSource = "nixos-unstable";
    };
  } (
    name: { nixpkgsSource }:


      dimension "System" {
        # TODO https://github.com/hercules-ci/hercules-ci-agent/issues/256
        "aarch64-linux" = { enable = name != "nixos-unstable"; };
        "x86_64-linux" = { enable = true; };
        "x86_64-darwin" = { enable = true; };
      } (
        system: { enable }:
          lib.optionalAttrs enable (
            import ../default.nix {
              inherit nixpkgsSource system allTargets;
            }
          )
      )
  );
in
allTargets
