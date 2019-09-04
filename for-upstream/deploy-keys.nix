{ config, lib, options, pkgs, ... }:
let
  cfg = config.services.hercules-ci-agent;
  inherit (lib) mkIf mkOption types;

  binaryCachesPath = cfg.finalConfig.binaryCachesPath or null;
  binaryCachesDir = lib.removeSuffix "binary-caches.json" binaryCachesPath;
  binaryCachesCorrect = lib.hasSuffix "binary-caches.json" binaryCachesPath;

  clusterJoinTokenPath = cfg.finalConfig.clusterJoinTokenPath or null;
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
      description = ''
        NOTE: This option works with NixOps only.

        A binary-caches.json to deploy.

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

      deployment.keys."binary-caches.json" =
        mkIf (cfg.binaryCachesFile != null) {
          user = config.services.hercules-ci-agent.user;
          destDir = binaryCachesDir;
          keyFile = cfg.binaryCachesFile;
        };

      # Add explicit default => binaryCachesPath will be set => missing file will be an error.
      services.hercules-ci-agent.extraOptions.binaryCachesPath =
        mkIf (cfg.binaryCachesFile != null) (
          lib.mkDefault (cfg.secretsDirectory + "/binary-caches.json")
        );

    }
    else {
      assertions = [
        {
          assertion = cfg.binaryCachesFile == null;
          message = ''
            The option services.hercules-ci-agent.binaryCachesFile only works in NixOps.
            Please deploy your binary-caches.json file with some other means.
          '';
        }
      ];
    };
}
