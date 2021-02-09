{ pkgs }:
let
  flakeCompat = import ../nix/flake-compat.nix;
  flake = flakeCompat.defaultNix;
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
