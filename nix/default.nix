{ sources ? import ./sources.nix
, nixpkgsSource ? "nixos-18.09"
, nixpkgs ? sources."${nixpkgsSource}"
}:
let
  dev-overlay = self: pkgs: {
    devTools = {
      inherit (pkgs)
        shellcheck
        ;
      inherit (import sources.niv {})
        niv
        ;
      inherit (pkgs.haskellPackages)
        brittany
        ;
    };
  };
in
import nixpkgs { overlays = [ (import ./overlay.nix) dev-overlay ] ; config = {}; }
