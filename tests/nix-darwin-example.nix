{ flake ? (import ../nix/flake-compat.nix).defaultNix, pkgs }:
let
  nix-darwin = flake.inputs.nix-darwin;
  inherit (pkgs) system;
  configuration = {
    imports = [
      flake.darwinModules.agent-profile
    ];
    services.hercules-ci-agent.enable = true;
  };
  nixpkgs = pkgs.path;
  machine = import nix-darwin {
    inherit nixpkgs system configuration;
  };
in
machine.system
