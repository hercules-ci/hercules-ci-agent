{ sources ? import ./sources.nix
, nixpkgsSource ? "nixos-20.09"
, nixpkgs ? sources.${nixpkgsSource}
  # Sharing the test suite
, allTargets ? import ./ci.nix
, testSuiteTarget ? "nixos-20_09"
, testSuitePkgs ? allTargets.${testSuiteTarget}.${system}
, system ? builtins.currentSystem
}:

let
  dev-and-test-overlay =
    self: pkgs:
      {
        inherit testSuitePkgs;
        devTools =
          {
            inherit (self.hercules-ci-agent-packages.internal.haskellPackages)
              ghc
              ghcid
              ;
            inherit (pkgs)
              jq
              cabal2nix
              nix-prefetch-git
              niv
              ;
            inherit pkgs;
          };
      };
  pkgs =
    import nixpkgs {
      overlays = [ (import ./overlay.nix) dev-and-test-overlay ];
      config = {};
      inherit system;
    };
in
pkgs
