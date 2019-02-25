{ sources ? import ./sources.nix
, nixpkgsSource ? "nixos-18.09"
, nixpkgs ? sources."${nixpkgsSource}"
}:
with
  { dev-overlay = self: pkgs:
      { inherit (import sources.niv {}) niv;
      };
  };
import nixpkgs { overlays = [ (import ./overlay.nix) dev-overlay ] ; config = {}; }
