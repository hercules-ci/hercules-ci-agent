{ nixpkgs }:
let
  pkgs = import nixpkgs {};
in {
  foo = pkgs.recurseIntoAttrs (
          pkgs.makeOverridable ({}: {}: { a = pkgs.zlib; }) {}
        ) // {
          overrideDerivation = null; # produces an abort in foo.overrideDerivation otherwise
        };
}
