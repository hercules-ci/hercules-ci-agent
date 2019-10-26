self: pkgs:

let
  sources = import ./sources.nix;
in
{
  # packages defined in this repo
  hercules-ci-agent-packages = pkgs.callPackages ./packages.nix {};

  # overrides
  hercules-ci-agent =
    pkgs.haskell.lib.justStaticExecutables self.hercules-ci-agent-packages.hercules-ci-agent;

  inherit (self.hercules-ci-agent-packages) pre-commit-check;

  toTOML-test = pkgs.callPackage ../for-upstream/to-toml/test-run.nix {};

  boehmgc-hercules = (
    pkgs.boehmgc.override {
      enableLargeConfig = true;
    }
  ).overrideAttrs (
    attrs: attrs
    // {
         patches = (attrs.patches or [])
           ++ [
                ./boehmgc-8.0.2-min-heap-incr-1mb.patch
              ]
           ;
       }
  );

  nix = pkgs.nix.override (
    args:
      args
      // {
           boehmgc = self.boehmgc-hercules;
         }
  );
}
