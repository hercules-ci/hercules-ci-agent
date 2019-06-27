self: pkgs:

let
  sources = import ./sources.nix;
in {
  # packages defined in this repo
  hercules-ci-agent-packages = pkgs.callPackages ./packages.nix {};

  # overrides
  hercules-ci-agent = pkgs.haskell.lib.justStaticExecutables self.hercules-ci-agent-packages.hercules-ci-agent;

  toTOML-test = pkgs.callPackage ../for-upstream/to-toml/test-run.nix {};
}
