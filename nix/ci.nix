let
  sources = import ./sources.nix;
  defaultNixpkgsSource = "nixos-20.03";

  lib = import (sources.${defaultNixpkgsSource} + "/lib");
  inherit (import sources."project.nix" { inherit lib; }) dimension;

  # nix-build doesn't traverse names with periods...
  allTargets = dimension "Nixpkgs version" {
    "nixos-20_03" = {
      nixpkgsSource = "nixos-20.03";
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
            import ../default.nix {
              inherit nixpkgsSource system allTargets;
            }
          )
      )
  );
in
allTargets
