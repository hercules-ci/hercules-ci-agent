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
          nativeBuildInputs = [ ];
          __hci_mounts = builtins.toJSON {
            "/etc/hosts" = "hosts";
          };
          __hci_effect_virtual_uid = 42;
          __hci_effect_virtual_gid = 142;
          __hci_effect_root_read_only = true;
        } ''
          set -x
          ! echo hi >/hi

          [[ $(id -u) == 42 ]]
          [[ $(id -g) == 142 ]]

          set +x

          grep '"hercules-ci":' $HERCULES_CI_SECRETS_JSON

          echo all good.
        '' // { isEffect = true; };
      };
  };
}
