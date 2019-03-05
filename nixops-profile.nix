/*
  Like the upstream NixOps profile in nixpkgs, but bleeding edge.

  Importing this file as a NixOS module in NixOps is equivalent to loading the
  upstream NixOps profile from NixOS *after* this version is released.

  The purpose of the profile is to provide sane defaults for an entire
  agent machine, deployed with NixOps, whereas the NixOS *module* only
  adds the agent service.

 */
{
  # Override hercules-ci-agent to the version in this repo.
  nixpkgs.overlays = [ (import ./nix/overlay.nix) ];

  imports = [
    ./for-upstream-only/nixos/profiles/nixops-hercules-ci-agent.nix
    ./for-upstream-only/nixos/modules/services/continuous-integration/hercules-ci-agent.nix
  ];
}
