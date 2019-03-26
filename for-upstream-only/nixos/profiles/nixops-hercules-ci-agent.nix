/*
  A NixOS profile that provides defaults for machines that serve as a
  Hercules CI agent, deployed with NixOps.
 */
{ config, ... }:
{
  imports = [ ./hercules-ci-agent.nix ];

  services.hercules-ci-agent.clusterJoinTokenPath = "/var/lib/keys/hercules-ci-agent/agent-token.key";

  users.extraUsers.hercules-ci-agent.extraGroups = [ "keys" ];

  deployment.keys."agent-token.key" = {
    user = config.services.hercules-ci-agent.user;
    destDir = "/var/lib/keys/hercules-ci-agent";
  };

}
