{ lib, config, self, ... }:
let
  inherit (lib) mkOption types;
  location = self + ("/" + config.privateDevInputSubflakePath);
  narHashPhysical = location + ".narHash";
  narHashRelative = config.privateDevInputSubflakePath + ".narHash";
  narHash =
    lib.strings.replaceStrings
      [ "\n" "\r" ]
      [ "" "" ]
      (builtins.readFile narHashPhysical);

in
{
  imports = [
    ./flake-partitions.nix
  ];
  options = {
    privateDevInputSubflakePath = lib.mkOption {
      type = types.str;
      description = ''
        Relative path string to a flake containing development inputs.

        If `pre-commit-hooks.nix` is loaded, it will be used to maintain a similarly named `.narHash` file.
        This file is needed for nix to be able to load the subflake in pure mode, as of Nix 2.16.
      '';
      example = "dev/private";
    };
  };
  config = {
    partitions.dev.extraInputsFlake = "path:${builtins.unsafeDiscardStringContext location}?narHash=${narHash}";
    partitions.dev.settings = { inputs, ... }: {
      perSystem = { pkgs, options, ... }: {
        config = lib.mkIf (options?pre-commit) {
          pre-commit.settings.hooks.dev-private-narHash = {
            enable = true;
            description = "dev-private-narHash";
            entry = "sh -c '${lib.getExe pkgs.nix} --extra-experimental-features nix-command hash path ${config.privateDevInputSubflakePath} >${narHashRelative}'";
          };
        };
      };
    };
  };
}
