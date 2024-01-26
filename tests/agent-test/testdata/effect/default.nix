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
          __hci_mounts = builtins.toJSON {
            "/etc/forwarded-path" = "forwarded-path";
            "/var/lib/shared-data" = "shared-data";
          };
        } ''
          echo 1>&2 hello "on stderr"
          echo hello "on stdout"
          cat ${builtins.toFile "the-src" "hi from src\n"}
          cat /etc/forwarded-path/hello
          echo 'hello from shared-data' > /var/lib/shared-data/hello
          cat /var/lib/shared-data/hello

          echo -n log line "without newline"
        '' // { isEffect = true; };
      };
  };
}
