{ config, lib, options, pkgs, ... }:
let
  cfg = config.services.hercules-ci-agent;
  inherit (lib) mkIf mkOption types;

  binaryCachesPath = cfg.effectiveConfig.binaryCachesPath;
  binaryCachesDir = lib.removeSuffix "binary-caches.json" binaryCachesPath;
  binaryCachesCorrect = lib.hasSuffix "binary-caches.json" binaryCachesPath;

  clusterJoinTokenPath = cfg.effectiveConfig.clusterJoinTokenPath;
  clusterJoinTokenDir = lib.removeSuffix "cluster-join-token.key" clusterJoinTokenPath;
  clusterJoinTokenCorrect = lib.hasSuffix "cluster-join-token.key" clusterJoinTokenPath;

in
{
  options.services.hercules-ci-agent = {

    enableKeyDeployment = mkOption {
      type = types.bool;
      default = options ? deployment.keys;
      defaultText = "true if deployment.keys is available, false otherwise";
      description = ''
        When enabled, configure secrets deployment via the NixOps (or compatible)
        deployment.keys options.
      '';
    };

    binaryCachesFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      visible = false;
      description = ''
        The option services.hercules-ci-agent.binaryCachesFile has been removed.
        binary-caches.json can now be deployed like an other secrets file.
        It does not require special treatment during deployment.

        For the format, see https://docs.hercules-ci.com/hercules-ci/reference/agent-config/#binaryCachesPath
      '';
    };

  };

  config =
    # We can only define values if the corresponding options exist.
    # This normal conditional works because (most of) options can be evaluated
    # before config.
    if options ? deployment.keys
    then mkIf (cfg.enable && cfg.enableKeyDeployment) {
      assertions = [
        {
          assertion = cfg.binaryCachesFile == null;
          message = ''
            The option services.hercules-ci-agent.binaryCachesFile has been removed,
              because the binary-caches.json does not need special treatment during
              deployment anymore. You're using the builtin key deployment.

              Please rename your definition of:
                services.hercules-ci-agent.binaryCachesFile = ...;
              to:
                deployment.keys."binary-caches.json".keyFile = ...;

          '';
        }


        {
          assertion = (binaryCachesPath != null) -> binaryCachesCorrect;
          message = ''
            The Hercules CI Agent's NixOps keys integration module does not
            currently support arbitrary file names for the binary-caches.json
            deployment, because we have had issues with the NixOps keys "path"
            attribute.
          '';
        }
        {
          assertion = (clusterJoinTokenPath != null) -> clusterJoinTokenCorrect;
          message = ''
            The Hercules CI Agent's NixOps keys integration module does not
            currently support arbitrary file names for the cluster-join-token.key
            deployment, because we have had issues with the NixOps keys "path"
            attribute.
          '';
        }
      ];

      users.extraUsers.hercules-ci-agent.extraGroups = [ "keys" ];

      deployment.keys."cluster-join-token.key" = {
        user = config.services.hercules-ci-agent.user;
        destDir = clusterJoinTokenDir;
      };

      deployment.keys."binary-caches.json" = {
        user = config.services.hercules-ci-agent.user;
        destDir = binaryCachesDir;
      };

    }
    else {
      assertions = [
        {
          assertion = cfg.binaryCachesFile == null;
          message = ''
            The option services.hercules-ci-agent.binaryCachesFile has been removed,
              because the binary-caches.json can now be deployed like any other secrets file.

              Please
                - remove the services.hercules-ci-agent.binaryCachesFile value
                - make sure you deploy a binary-caches.json file to your agent's
                    ${cfg.effectiveConfig.binaryCachesPath}

              For the format, see https://docs.hercules-ci.com/hercules-ci/reference/agent-config/#binaryCachesPath
          '';
        }
      ];
    };
}
