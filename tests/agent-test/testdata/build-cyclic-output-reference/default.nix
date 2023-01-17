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
      let pkgs = import nixpkgs { };
      in {
        it = pkgs.runCommand "cyclic"
          {
            t = builtins.currentTime;
            outputs = [ "out" "outputOne" "outputTwo" ];
          } ''
          echo $outputOne >$outputTwo
          echo $outputTwo >$outputOne
          touch $out
        '';
      };
  };
}
