/*
  A NixOS profile that provides defaults for machines that serve as a
  Hercules CI agent, deployed with NixOps.
 */
{ config, lib, pkgs, ... }:
let
  cfg = config.profile.hercules-ci-agent;
  inherit (lib) mkIf;

  cfgHasCachix = cfg.cachixSecretsFile != null;

in
{
  imports = [ ./hercules-ci-agent.nix ];

  config = {
    users.extraUsers.hercules-ci-agent.extraGroups = [ "keys" ];

    services.hercules-ci-agent.clusterJoinTokenPath = "/var/lib/keys/hercules-ci-agent/agent-token.key";
    deployment.keys."agent-token.key" = {
      user = config.services.hercules-ci-agent.user;
      destDir = "/var/lib/keys/hercules-ci-agent";
    };

    profile.hercules-ci-agent.cachixDeployedSecretsPath = mkIf cfgHasCachix
      "/var/lib/keys/hercules-ci-agent/cachix.jsonl.key";
    deployment.keys."cachix.jsonl.key" = mkIf cfgHasCachix {
      user = config.services.hercules-ci-agent.user;
      destDir = "/var/lib/keys/hercules-ci-agent";
      keyFile = cfg.cachixSecretsFile;
    };

  };

}
