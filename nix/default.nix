{ flake ? (import ./flake-compat.nix).defaultNix
, nixpkgs ? flake.inputs.nixpkgs
  # Sharing the test suite
, allTargets ? import ./ci.nix
, testSuiteTarget ? "nixos-20_03"
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
              stack
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
      overlays = [ (import ./overlay.nix flake.inputs) dev-and-test-overlay ];
      config = {};
      inherit system;
    };
in
pkgs
