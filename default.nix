attrs@{ ... }:

let
  pkgs = import ./nix attrs;

in

pkgs.recurseIntoAttrs {
  inherit (pkgs)
    hercules-ci-agent
    hercules-ci-agent-packages
    devTools
    toTOML-test
    ;
}
