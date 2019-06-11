/*
  A NixOS profile that provides defaults for machines that serve as a
  Hercules CI agent.
 */
{ pkgs, lib, config, ...}:
let
  cfg = config.profile.hercules-ci-agent;
  inherit (lib) types isAttrs mkIf mkDefault escapeShellArg;

  ifHasCacheKeys = mkIf (cfg.cacheKeysFile != null || cfg.cacheKeysDeployedPath != null);

  # Don't set it or the cache-keys-json module will emit confusing errors. This module
  # should fully wrap this aspect of deployment to avoid mistakes.
  # Reusing that module's config is a bad idea because the intent is different.
  ifHasCacheKeysAndOk = mkIf (cfg.cacheKeysFile != null && cfg.cacheKeysDeployedPath != null);

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
    profile.hercules-ci-agent.cacheKeysDeployedPath = lib.mkOption {
      type = types.nullOr types.str;
      default = null;
      # TODO (doc) CacheKeys format reference link
      description = ''
        Separately deployed file that configures the agent to use specified caches.

        If not null, this option must point to a CacheKeys JSON file on the
        deployed machine by means of a literal string, in order to avoid putting
        secrets in the Nix store.

        The file must be readable by the hercules-ci-agent user. Part of the
        file will be made available to the Nix daemon exclusively.

        Sets both profile.cacheKeys.deployedPath and
        services.hercules-ci-agent.cacheKeysPath.
      '';
    };

    profile.hercules-ci-agent.cacheKeysFile = lib.mkOption {
      type = types.nullOr types.path;
      default = null;
      # TODO (doc) CacheKeys format reference link
      description = ''
        File that configures the agent to use specified caches.

        A CacheKeys JSON file. It
        will be read during evaluation; this should be a local file reference.

        If you're using the NixOps profile, this option is sufficient to
        configure the Cachix integration.

        If you're using only the generic profile, you will need to take care of
        the deployment of this file, so that it is available at the path configured in
        profile.hercules-ci-agent.cacheKeysDeployedPath.
      '';
    };

  };

  imports = [ ./cache-keys-json.nix ];

  config = {
    services.hercules-ci-agent.enable = true;
    services.hercules-ci-agent.cacheKeysPath = cfg.cacheKeysDeployedPath;
    systemd.services.hercules-ci-agent.after = ifHasCacheKeys [ "cache-keys-install.service" ];

    profile.cacheKeys.file = ifHasCacheKeysAndOk cfg.cacheKeysFile;
    profile.cacheKeys.deployedPath = ifHasCacheKeysAndOk cfg.cacheKeysDeployedPath;

    nix.gc.automatic = true;
    nix.gc.options = ''--max-freed "$((${toString config.profile.hercules-ci-agent.freespaceGB} * 1024**3 - 1024 * $(df -P -k /nix/store | tail -n 1 | ${pkgs.gawk}/bin/awk '{ print $4 }')))"'';

    # These are essentially duplications of the cache-keys-json module, but doing so is
    # required in order to provide the right error messages.
    assertions = ifHasCacheKeys [
      {
        assertion = cfg.cacheKeysDeployedPath != null -> cfg.cacheKeysFile != null;
        message = ''
          You need to specify profile.hercules-ci-agent.cacheKeysFile in
          order to provide the non-sensitive parts of the cache configuration.

          WARNING: If you've used a path expression in
          profile.hercules-ci-agent.cacheKeysDeployedPath, you may want to
          delete it from your Nix store!
        '';
      }

      # TODO: not required when file is not sensitive.
      {
        assertion = cfg.cacheKeysFile != null -> cfg.cacheKeysDeployedPath != null;
        message = ''
          You need to deploy the CacheKeys file to the machine outside the
          Nix store and set profile.hercules-ci-agent.cacheKeysDeployedPath to
          the location of the deployed file.
        '';
      }
    ];
  };
}
