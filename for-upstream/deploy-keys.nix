{ config, lib, options, pkgs, ... }:
let
  cfg = config.services.hercules-ci-agent;
  inherit (lib) mkIf types;

  binaryCachesPath = cfg.finalConfig.binaryCachesPath or null;
  binaryCachesDir = lib.removeSuffix "binary-caches.json" binaryCachesPath;
  binaryCachesCorrect = lib.hasSuffix "binary-caches.json" binaryCachesPath;

  clusterJoinTokenPath = cfg.finalConfig.clusterJoinTokenPath or null;
  clusterJoinTokenDir = lib.removeSuffix "cluster-join-token.key" clusterJoinTokenPath;
  clusterJoinTokenCorrect = lib.hasSuffix "cluster-join-token.key" clusterJoinTokenPath;

  # A two-stage mkIf, to enable a configuration only when options are available.
  ifMkIf = c1: c2: a: if c1 then mkIf c2 a else {};

in
{
  options.services.hercules-ci-agent = {
    enableKeyDeployment = lib.mkOption {
      type = types.bool;
      default = options ? deployment.keys;
      defaultText = "true if deployment.keys is available, false otherwise";
      description = ''
        When enabled, configure secrets deployment via the NixOps (or compatible)
        deployment.keys options.
      '';
    };
  };

  config = ifMkIf (options ? deployment.keys) (cfg.enable && cfg.enableKeyDeployment) {
    assertions = [
      {
        assertion = (binaryCachesPath != null) -> binaryCachesCorrect;
        message = ''
          The Hercules CI Agent's NixOps keys integration module does not
          currently support arbitrary file names for the binary-caches.json.key
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

    deployment.keys."binary-caches.json" = mkIf (binaryCachesPath != null) {
      user = config.services.hercules-ci-agent.user;
      destDir = binaryCachesDir;
      keyFile = cfg.binaryCachesFile;
    };

  };

}
