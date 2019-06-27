/*
  Like the upstream NixOS module in nixpkgs, but for the version of the agent
  that this file was bundled with.
 */
{
  imports = [ ./for-upstream/default.nixos.nix ];

  # Overrides hercules-ci-agent to the version in this repo.
  nixpkgs.overlays = [ (import ./nix/overlay.nix) ];

  # TODO blacklist the upstream module (does that blacklist imports transitively?...)
}
