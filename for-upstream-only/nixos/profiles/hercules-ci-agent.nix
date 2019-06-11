/*
  A NixOS profile that provides defaults for machines that serve as a
  Hercules CI agent.
 */
{ pkgs, lib, config, ...}:
let
  cfg = config.profile.hercules-ci-agent;
  inherit (lib) types isAttrs mkIf mkDefault escapeShellArg;

  ifCachix = mkIf (cfg.cachixSecretsFile != null || cfg.cachixDeployedSecretsPath != null);

  # Don't set it or the cachix module will emit confusing errors. This module
  # should fully wrap this aspect of deployment to avoid mistakes.
  ifCachixAndOk = mkIf (cfg.cachixSecretsFile != null && cfg.cachixDeployedSecretsPath != null);

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

    # This option is replicated here in order to improve error messages.
    profile.hercules-ci-agent.cachixDeployedSecretsPath = lib.mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        If not null, this option must point to a file on the deployed machine
        by means of a literal string, in order to avoid putting secrets in the
        Nix store.

        The file must be readable by the hercules-ci-agent user. Part of the
        file will be made available to the Nix daemon exclusively.

        Sets both nix.cachix.deployedSecretsPath and
        services.hercules-ci-agent.cachixSecretsPath.
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

  imports = [ ./cachix-via-json.nix ];

  config = {
    services.hercules-ci-agent.enable = true;
    services.hercules-ci-agent.cachixSecretsPath = cfg.cachixDeployedSecretsPath;
    systemd.services.hercules-ci-agent.after = ifCachix [ "cachix-install-netrc.service" ];

    nix.cachix.secretsFile = ifCachixAndOk cfg.cachixSecretsFile;
    nix.cachix.deployedSecretsPath = ifCachixAndOk cfg.cachixDeployedSecretsPath;

    nix.gc.automatic = true;
    nix.gc.options = ''--max-freed "$((${toString config.profile.hercules-ci-agent.freespaceGB} * 1024**3 - 1024 * $(df -P -k /nix/store | tail -n 1 | ${pkgs.gawk}/bin/awk '{ print $4 }')))"'';

    # These are essentially duplications of the cachix module, but doing so is
    # required in order to provide the right error messages.
    assertions = ifCachix [
      {
        assertion = cfg.cachixDeployedSecretsPath != null -> cfg.cachixSecretsFile != null;
        message = ''
          You need to specify profile.hercules-ci-agent.cachixSecretsFile in
          order to provide the non-sensitive parts of the cache configuration.

          WARNING: If you've used a path expression in
          profile.hercules-ci-agent.cachixDeployedSecretsPath, you may want to
          delete it from your Nix store!
        '';
      }

      # TODO: not required when file is not sensitive.
      {
        assertion = cfg.cachixSecretsFile != null -> cfg.cachixDeployedSecretsPath != null;
        message = ''
          You need to deploy the Cachix secrets file to the machine outside the
          Nix store and set profile.hercules-ci-agent.cachixDeployedSecretsPath to
          the location of the deployed file.
        '';
      }
    ];
  };
}
