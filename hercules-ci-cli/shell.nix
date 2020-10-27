let
  pkgs = import ../nix { };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.stack
    pkgs.haskellPackages.ghcid
  ];
}
