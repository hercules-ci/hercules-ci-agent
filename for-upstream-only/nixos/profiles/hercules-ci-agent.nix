/*
  A NixOS profile that provides defaults for machines that serve as a
  Hercules CI agent.
 */
{ pkgs, lib, config, ...}:
let
  cfg = config.profile.hercules-ci-agent;
  inherit (lib) types isAttrs mkIf escapeShellArg;

  ifCachix = mkIf (cfg.cachixSecretsFile != null);

in
{
  options = {
    profile.hercules-ci-agent.freespaceGB = lib.mkOption {
      type = types.int;
      default = 30;
      description = ''
        Amount of free space (GB) to ensure on garbage collection
      '';
    };
    profile.hercules-ci-agent.cachixSecretsFile = lib.mkOption {
      type = types.nullOr types.path;
      default = null;
      description = ''
        A JSON Lines file produced by the cachix export command. It
        will be read during evaluation; this should be a local file reference.

        If you're using the NixOps profile, this option is sufficient to
        configure the Cachix integration.

        If you're using only the generic profile, this file will be used to
        configure Cachix read access. You will need to take care of the deployment
        of this file, so that it is available at the path configured in
        services.hercules-ci-agent.cachixSecretsPath.
      '';
    };
  };

  imports = [ ./cachix-use-export.nix ];

  config = {
    services.hercules-ci-agent.enable = true;

    nix.cachix.secretsFile = ifCachix cfg.cachixSecretsFile;
    nix.cachix.deployedSecretsPath = ifCachix config.services.hercules-ci-agent.cachixSecretsPath;

    nix.gc.automatic = true;
    nix.gc.options = ''--max-freed "$((${toString config.profile.hercules-ci-agent.freespaceGB} * 1024**3 - 1024 * $(df -P -k /nix/store | tail -n 1 | ${pkgs.gawk}/bin/awk '{ print $4 }')))"'';

  };
}
