{ sources ? import ./sources.nix
, nixpkgsSource ? "nixos-18.09"
, nixpkgs ? sources."${nixpkgsSource}"
}:
with
  { dev-overlay = self: pkgs:
      { inherit (import sources.niv {}) niv;
      };
    sources-overlay = self: pkgs: { inherit sources; };
  };
import nixpkgs { overlays = [ sources-overlay (import ./overlay.nix) dev-overlay ] ; config = {}; }
