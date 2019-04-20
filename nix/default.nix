{ sources ? import ./sources.nix
, nixpkgsSource ? "nixos-19.03"
, nixpkgs ? sources."${nixpkgsSource}"

  # Sharing the test suite
, allTargets ? import ../default.nix
, testSuiteTarget ? "nixos-19_03"
, testSuitePkgs ? allTargets."${testSuiteTarget}"
}:
let
  dev-and-test-overlay = self: pkgs: {

    inherit testSuitePkgs;

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
import nixpkgs { overlays = [ (import ./overlay.nix) dev-and-test-overlay ] ; config = {}; }
