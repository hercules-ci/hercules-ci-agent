{ ghc }:
let
  pkgs = import ./default.nix { };
in
pkgs.haskell.lib.buildStackProject {
  # TODO: revert this to
  # inherit ghc;
  ghc = pkgs.haskell.compiler.ghc8104;
  name = "hercules-ci-stack-shell";
  buildInputs = [
    # Static linking avoids the error
    # error: undefined reference to 'pthread_sigmask', version 'GLIBC_2.32'
    (pkgs.lzma.override { enableStatic = true; })

    pkgs.zlib
    pkgs.openssl
    pkgs.nix
    pkgs.nlohmann_json
    pkgs.boost
  ];
  # Block any inherited shellHook when nesting shells without nix-shell --pure,
  # because we must be quite for HIE to work.
  shellHook = ":";
}
