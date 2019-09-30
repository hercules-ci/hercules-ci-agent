let
  sources = import ./sources.nix;
  defaultNixpkgsSource = "nixos-19.03";

  lib = import (sources.${defaultNixpkgsSource} + "/lib");
  inherit (import sources."project.nix" { inherit lib; }) dimension;

  # nix-build doesn't traverse names with periods...
  allTargets = dimension "Nixpkgs version" {
    "nixos-19_03" = {
      nixpkgsSource = "nixos-19.03";
    };
    "nixos-unstable" = {
      nixpkgsSource = "nixos-unstable";
    };
  } (
    _name: { nixpkgsSource }:


      dimension "System" {
        "x86_64-linux" = { enable = true; };
        # TODO: darwin/unstable blocked on inline-c-cpp update
        "x86_64-darwin" = { enable = nixpkgsSource != "nixos-unstable"; };
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
