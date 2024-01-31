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
          nativeBuildInputs = [ pkgs.curl ];
          __hci_mounts = builtins.toJSON {
            "/etc/forwarded-path" = "forwarded-path";
            "/var/lib/shared-data" = "shared-data";
            "/etc/hosts" = "hosts";
          };
        } ''
          echo 1>&2 hello "on stderr"
          echo hello "on stdout"
          cat ${builtins.toFile "the-src" "hi from src\n"}
          cat /etc/forwarded-path/hello
          echo 'hello from shared-data' > /var/lib/shared-data/hello
          cat /var/lib/shared-data/hello

          cat /etc/resolv.conf
          cat /etc/hosts
          curl --fail -v --no-progress-bar \
              $HERCULES_CI_API_BASE_URL/hello

          set -x
          [[ $(id -u) == 0 ]]
          [[ $(id -g) == 0 ]]
          echo hi >/hi
          set +x

          grep '"hercules-ci":' $HERCULES_CI_SECRETS_JSON

          echo -n log line "without newline"
        '' // { isEffect = true; };
      };
  };
}
