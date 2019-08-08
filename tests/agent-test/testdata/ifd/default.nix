{ nixpkgs ? null }:
let
  pkgs = import nixpkgs {};
  helloIfdNix =
    pkgs.runCommand
      "hello-ifd.nix"
      {
        code = "pkgs: pkgs.hello";
      }
      ''echo "$code" >$out
      '';
in
{
  helloIfd = import helloIfdNix pkgs;
}
