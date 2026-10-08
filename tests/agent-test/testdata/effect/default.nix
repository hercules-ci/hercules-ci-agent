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
          nativeBuildInputs = [ pkgs.curl pkgs.jq pkgs.nix ];
          __hci_effect_mounts = builtins.toJSON {
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

          # Nix must recognize the store as a daemon store even if NIX_REMOTE is
          # unset, as some tools will "helpfully" clear environment variables.
          [[ ! -w /nix/var/nix ]]
          storeUrl="$(env -u NIX_REMOTE nix --extra-experimental-features nix-command store info --json | jq -r .url)"
          [[ "$storeUrl" == daemon ]]

          # Profiles are usable transiently, the default one and an explicit one
          HOME=/build nix-env --set ${pkgs.curl}
          [[ "$(readlink -f /nix/var/nix/profiles/default)" == "${pkgs.curl}" ]]
          HOME=/build nix-env -p /nix/var/nix/profiles/per-user/root/hci-test --set ${pkgs.curl}
          [[ "$(readlink -f /nix/var/nix/profiles/per-user/root/hci-test)" == "${pkgs.curl}" ]]
          set +x

          grep '"hercules-ci":' $HERCULES_CI_SECRETS_JSON

          echo -n log line "without newline"
        '' // { isEffect = true; };
      };
  };
}
