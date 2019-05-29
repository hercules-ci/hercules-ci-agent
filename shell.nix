{ sources ? import ./nix/sources.nix
, nixpkgsSource ? "nixos-19.03"
, nixpkgs ? sources."${nixpkgsSource}"
}:

let
  pkgs = import nixpkgs { overlays = [ (import ./nix/overlay.nix) ] ; config = {}; };
  agentpkgs = import ./default.nix {};
in pkgs.mkShell {
  buildInputs = [ agentpkgs.devTools.niv agentpkgs.devTools.shellcheck ];
}
