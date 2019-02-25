{ sources ? import ./sources.nix
, nixpkgsSource ? "nixos-18.09"
, nixpkgs ? sources."${nixpkgsSource}"
}:
with
  { overlay = self: pkgs:
      { inherit (import sources.niv {}) niv;
        inherit nixpkgsSource;
        packages = pkgs.callPackages ./packages.nix {};

        hercules-ci-agent = pkgs.haskell.lib.justStaticExecutables self.packages.hercules-ci-agent;
      };
  };
import nixpkgs { overlays = [ overlay ] ; config = {}; }
