{ src ? null }:
{
  herculesCI.onPush.cd = {
    inputs =
      # {
      #   nixpkgs = {
      #     project = "nixpkgs";
      #   };
      # };
      throw "not part of the test (but could be if needed)";

    outputs = { nixpkgs, ... }:
      let pkgs = import nixpkgs {};
      in {
        effects.launchIt = pkgs.runCommand "one" {
          t = builtins.currentTime;
        } ''
          echo 1>&2 hello "on stderr"
          echo hello "on stdout"
          cat ${builtins.toFile "the-src" "hi from src\n"}

          echo -n log line "without newline"
        '' // { isEffect = true; };
      };
  };
}
