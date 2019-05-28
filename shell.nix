{ sources ? import ./nix/sources.nix
, nixpkgsSource ? "nixos-19.03"
, nixpkgs ? sources."${nixpkgsSource}"
}:

let
  pkgs = import nixpkgs { overlays = [ (import ./nix/overlay.nix) ] ; config = {}; };
in pkgs.mkShell {
  buildInputs = [ pkgs.devTools.niv pkgs.devTools.shellcheck ];
}
