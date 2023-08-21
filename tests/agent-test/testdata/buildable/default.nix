{ src ? null }:
{ 
  herculesCI.onPush.ci = {
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
        it = pkgs.runCommand "one" { t = builtins.currentTime; } ''
          echo 1>&2 hello on stderr
          echo hello on stdout
          echo ${builtins.toFile "the-src" "hi"} >$out
        '';
      };
  };
}
