{ pkgs }:

let
  eval = import (pkgs.path + "/nixos/lib/eval-config.nix") {
    baseModules = [
      ../module.nix
    ];
    modules = [];
  };
  options = pkgs.nixosOptionsDoc {
    options = eval.options;
  };

in {
  options = pkgs.writeText "agent-options" ''
  = NixOS Options

  ${options.optionsAsciiDoc}
  '' ;
}
