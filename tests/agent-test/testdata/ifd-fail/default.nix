{ nixpkgs ? null }:
let
  pkgs = import nixpkgs {};
  helloIfdNix =
    pkgs.runCommand
      "ifd-1.nix"
      {
        code = "pkgs: pkgs.hello";
      }
      ''echo "$code" >$out
      '';
  figletIfdNix =
    pkgs.runCommand
      "ifd-2.nix"
      {
        code = "pkgs: pkgs.figlet";
      }
      ''echo "This is a build failure for testing purposes." 1>&2
        exit 1
      '';
in
{
  figletIfd = import figletIfdNix pkgs;
  helloIfd = import helloIfdNix pkgs;
}
