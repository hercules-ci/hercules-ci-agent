{ sources ? import ./nix/sources.nix
, nixpkgsSource ? "nixos-19.03"
, nixpkgs ? sources."${nixpkgsSource}"
  # Sharing the test suite
, allTargets ? import ./nix/ci.nix
, testSuiteTarget ? "nixos-19_03"
, testSuitePkgs ? allTargets."${testSuiteTarget}"
}:

let
  dev-and-test-overlay = self: pkgs: {
    inherit testSuitePkgs;
  };
  pkgs = import nixpkgs { overlays = [ (import ./nix/overlay.nix) dev-and-test-overlay ] ; config = {}; };
in pkgs.recurseIntoAttrs {
  inherit (pkgs) hercules-ci-agent hercules-ci-agent-packages;
  devTools = {
    inherit (pkgs)
      shellcheck
      ;
    inherit (import sources.niv {})
      niv
      ;
  };
}
