/*
  Like the upstream NixOS module in nixpkgs, but for the version of the agent
  that this file was bundled with.
 */
{ pkgs, ... }:

let
  agentpkgs = import ./. { system = pkgs.system; };
in
{
  imports = [ ./for-upstream/default.nixos.nix ];

  # This module replaces what's provided by NixOS
  disabledModules = [ "services/continuous-integration/hercules-ci-agent/default.nix" ];

  config = {
    services.hercules-ci-agent.package = agentpkgs.hercules-ci-agent;
  };

}
