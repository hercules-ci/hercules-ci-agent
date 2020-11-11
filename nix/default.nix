{ system ? builtins.currentSystem }:
(import ./flake-compat.nix).defaultNix.internal.pkgs.${system}
