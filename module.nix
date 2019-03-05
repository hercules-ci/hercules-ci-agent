/*
  Like the upstream NixOS module in nixpkgs, but bleeding edge.

  Importing this file as a NixOS module is equivalent to loading the upstream
  module from NixOS *after* this version is released.
 */
{
  # Override hercules-ci-agent to the version in this repo.
  nixpkgs.overlays = [ (import ./nix/overlay.nix) ];

  imports = [ ./for-upstream-only/nixos/modules/services/continuous-integration/hercules-ci-agent.nix ];
}
