{ ghc }:
let pkgs = import ./. {};
in pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "hercules-ci-stack-shell";
  buildInputs = [
    pkgs.lzma
    pkgs.zlib
    pkgs.openssl
    pkgs.nix
    pkgs.boost
  ];
}
