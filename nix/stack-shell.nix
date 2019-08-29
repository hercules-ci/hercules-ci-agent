{ ghc }:
let
  pkgs = (import ../. {}).devTools.pkgs;
in
pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "hercules-ci-stack-shell";
  buildInputs = [
    pkgs.lzma
    pkgs.zlib
    pkgs.openssl
    pkgs.nix
    pkgs.boost
  ];
  # Block any inherited shellHook when nesting shells without nix-shell --pure,
  # because we must be quite for HIE to work.
  shellHook = ":";
}
