/*
  A NixOS profile that provides defaults for machines that serve as a
  Hercules CI agent, deployed with NixOps.
 */
{ config, lib, pkgs, ... }:
let
  cfg = config.profile.hercules-ci-agent;

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

    services.hercules-ci-agent.cachixSecretsPath = "/var/lib/keys/hercules-ci-agent/cachix.jsonl.key";
    deployment.keys."cachix.jsonl.key" = {
      user = config.services.hercules-ci-agent.user;
      destDir = "/var/lib/keys/hercules-ci-agent";
      keyFile = if cfg.cachixSecretsFile == null
                then pkgs.writeText "empty" ""
                else cfg.cachixSecretsFile;

    };
  };

}
