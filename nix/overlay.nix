self: pkgs:
{
  # packages defined in this repo
  hercules-ci-agent-packages = pkgs.callPackages ./packages.nix {};

  # overrides
  hercules-ci-agent = pkgs.haskell.lib.justStaticExecutables self.hercules-ci-agent-packages.hercules-ci-agent;
}
