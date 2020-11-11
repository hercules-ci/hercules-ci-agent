{ pkgs ? (import ../nix { }) }:
let
  inherit (pkgs) haskellPackages lib;
  ghc = haskellPackages.ghcWithPackages (p: [ p.releaser p.protolude ]);
in
pkgs.mkShell {
  buildInputs = [
    ghc
    haskellPackages.cabal-install
  ];
}
