{ ghc }:
let
  pkgs = (import ../. {}).devTools.pkgs;
in
pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "hercules-ci-stack-shell";
  buildInputs = [

    # Static linking avoids the error
    # error: undefined reference to 'pthread_sigmask', version 'GLIBC_2.32'
    (pkgs.lzma.override { enableStatic = true; })

    pkgs.zlib
    pkgs.openssl
    pkgs.nix
    pkgs.boost
  ];
  # Block any inherited shellHook when nesting shells without nix-shell --pure,
  # because we must be quite for HIE to work.
  shellHook = ":";
}
