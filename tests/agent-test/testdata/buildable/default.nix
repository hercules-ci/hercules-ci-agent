{ nixpkgs ? null }:
let pkgs = import nixpkgs {};
in
{ it = pkgs.runCommand "one" {} "echo 1 >$out";
}
