{ pkgs ? import ./nix/default.nix {}
, ...
}:
let hs = pkgs.hercules-ci-agent-packages.internal.haskellPackages;
    inherit (pkgs.hercules-ci-agent-packages.internal.pkgs) devTools;
in hs.shellFor {
  packages = p: [
    p.hercules-ci-api-core
    p.hercules-ci-api
    p.hercules-ci-api-agent
    p.hercules-ci-agent
    p.hercules-ci-agent-test
  ];
  buildInputs = [
    pkgs.boost
    pkgs.nix
  ];
  nativeBuildInputs = [
    pkgs.yaml2json
    pkgs.jq
    # hs.stack
    (hs.cabal-install.overrideScope (self: super: { Cabal = self.Cabal_3_2_1_0 or super.Cabal; }))
    hs.haskell-language-server

    devTools.niv
    devTools.ghcid
    devTools.jq
    devTools.cabal2nix
    devTools.nix-prefetch-git
  ];
  inherit (pkgs.hercules-ci-agent-packages.pre-commit-check) shellHook;
}
