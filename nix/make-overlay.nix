inputs@{ ... }:
self: pkgs:

{
  # packages defined in this repo
  hercules-ci-agent-packages = pkgs.callPackages ./packages.nix { inherit (inputs) pre-commit-hooks-nix; };

  # overrides
  hercules-ci-agent =
    pkgs.haskell.lib.justStaticExecutables self.hercules-ci-agent-packages.hercules-ci-agent // {
      # Intentionally not setting passthru. If someone overrides this, `rev`
      # alone is not enough for troubleshooting; we need to know the whole expression.
      rev = inputs.self.rev or "";
    };

  inherit (self.hercules-ci-agent-packages) pre-commit-check;

  boehmgc-hercules = (
    pkgs.boehmgc.override {
      enableLargeConfig = true;
    }
  ).overrideAttrs (
    attrs: attrs
      // {
      patches = (attrs.patches or [ ])
        ++ [
        ./boehmgc-8.0.2-min-heap-incr-1mb.patch
      ]
      ;
    }
  );

  nix = pkgs.nixUnstable.override (
    args:
    args
    // {
      boehmgc = self.boehmgc-hercules;
    }
  );

}
