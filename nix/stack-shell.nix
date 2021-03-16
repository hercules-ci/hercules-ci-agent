{ ghc }:
let
  pkgs = import ./default.nix { };
in
pkgs.haskell.lib.buildStackProject {
  # Override ghc to get a working cabal with GHC 8.10.2
  # See https://github.com/commercialhaskell/stackage/issues/5762
  # TODO: when updating GHC, revert this to
  # inherit ghc;
  ghc = pkgs.haskell.compiler.ghc8104;
  name = "hercules-ci-stack-shell";
  buildInputs = [
    # Static linking avoids the error
    # error: undefined reference to 'pthread_sigmask', version 'GLIBC_2.32'
    (pkgs.lzma.override { enableStatic = true; })

    pkgs.zlib
    pkgs.openssl
    pkgs.nixUnstable
    pkgs.boost
  ];
  # Block any inherited shellHook when nesting shells without nix-shell --pure,
  # because we must be quite for HIE to work.
  shellHook = ":";
}
