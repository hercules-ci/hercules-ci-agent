{
  # Override hercules-ci-agent to the version in this repo.
  nixpkgs.overlays = [ (import ./nix/overlay.nix) ];

  imports = [ ./upstream-only-module.nix ];
}
