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
      ''echo "$code" >$out
      '';
in
{
  figletIfd = import figletIfdNix pkgs;
  helloIfd = import helloIfdNix pkgs;
}
