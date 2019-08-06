/*
  Like the upstream NixOS module in nixpkgs, but for the version of the agent
  that this file was bundled with.
 */
{ pkgs, ... }:

let
 agentpkgs = import ./. {};
in {
  imports = [ ./for-upstream/default.nixos.nix ];

  services.hercules-ci-agent.package = agentpkgs.hercules-ci-agent;

  # TODO blacklist the upstream module (does that blacklist imports transitively?...)
}
