{ nixpkgs ? null }:
let pkgs = import nixpkgs {};
in
{ it = pkgs.runCommand "one" {} ''
    echo 1>&2 hello on stderr
    echo hello on stdout
    echo 1 >$out
  '';
}
