let
  sources = import ./sources.nix;
  defaultNixpkgsSource = "nixos-19.09";

  lib = import (sources.${defaultNixpkgsSource} + "/lib");
  inherit (import sources."project.nix" { inherit lib; }) dimension;

  # nix-build doesn't traverse names with periods...
  allTargets = dimension "Nixpkgs version" {
    "nixos-19_09" = {
      nixpkgsSource = "nixos-19.09";
    };
    "nixos-unstable" = {
      nixpkgsSource = "nixos-unstable";
    };
  } (
    _name: { nixpkgsSource }:


      dimension "System" {
        "aarch64-linux" = { enable = true; };
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
