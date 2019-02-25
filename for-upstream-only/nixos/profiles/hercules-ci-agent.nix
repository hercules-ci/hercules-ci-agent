{ pkgs, lib, config, ...}:
{
  options = {
    profile.hercules-ci-agent.freespaceGB = lib.mkOption {
      type = lib.types.int;
      default = 30;
      description = ''
        Amount of free space (GB) to ensure on garbage collection
      '';
    };
  };

  config = {

    services.hercules-ci-agent.enable = true;

    nix.gc.automatic = true;
    nix.gc.options = ''--max-freed "$((${toString config.profile.hercules-ci-agent.freespaceGB} * 1024**3 - 1024 * $(df -P -k /nix/store | tail -n 1 | ${pkgs.gawk}/bin/awk '{ print $4 }')))"'';

  };
}
