self: pkgs:

let
  sources = import ./sources.nix;
in {
  # packages defined in this repo
  hercules-ci-agent-packages = pkgs.callPackages ./packages.nix {};

  # overrides
  hercules-ci-agent = pkgs.haskell.lib.justStaticExecutables self.hercules-ci-agent-packages.hercules-ci-agent;

  toTOML-test = pkgs.callPackage ../for-upstream/to-toml/test-run.nix {};

  boehmgc-hercules = (pkgs.boehmgc.override {
        enableLargeConfig = true;
      }).overrideAttrs (attrs: attrs // {
        CFLAGS = (attrs.CFLAGS or "") + " -DMAX_HEAP_SECTS=655360";
      });

  nix = (pkgs.nix.override (args: args // {
      boehmgc = self.boehmgc-hercules;
    })).overrideAttrs (attrs: attrs // {
      patches = (attrs.patches or []) ++ [
        ./nix-no-negative-caching.patch
      ];
    });
}
