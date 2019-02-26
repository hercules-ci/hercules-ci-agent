{
  # Override hercules-ci-agent to the version in this repo.
  nixpkgs.overlays = [ (import ./nix/overlay.nix) ];

  imports = [
    ./for-upstream-only/nixos/profiles/hercules-ci-agent.nix
    ./for-upstream-only/nixos/modules/services/continuous-integration/hercules-ci-agent.nix
  ];
}
