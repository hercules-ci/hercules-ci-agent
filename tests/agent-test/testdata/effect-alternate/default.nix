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
          __hci_effect_mounts = builtins.toJSON {
            "/etc/hosts" = "hosts";
            "/dev/the-blocks" = "my-blocks";
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

          # Add username to /etc/passwd
          echo "hci-effect:x:42:142:hci-effect:$PWD:/bin/nologin" >> /etc/passwd
          echo "hci-effect:x:142:" >> /etc/group

          # Check that it contains the data that was written by the setup in the
          # NixOS VM test testScript.
          read DATA </dev/the-blocks
          [[ "$DATA" == "initial data of the loopback block device" ]]

          # Check that it's writable
          echo new-data >/dev/the-blocks
          read DATA </dev/the-blocks
          [[ "$DATA" == "new-data" ]]

          grep '"hercules-ci":' $HERCULES_CI_SECRETS_JSON

          echo all good.
        '' // { isEffect = true; };
      };
  };
}
