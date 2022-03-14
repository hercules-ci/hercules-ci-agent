flake:
self: pkgs:

{
  # packages defined in this repo
  hercules-ci-agent-packages = pkgs.callPackages ./packages.nix { inherit (flake.inputs) pre-commit-hooks-nix; inherit flake; };

  # overrides
  hercules-ci-agent =
    pkgs.haskell.lib.justStaticExecutables self.hercules-ci-agent-packages.hercules-ci-agent // {
      # Intentionally not setting passthru. If someone overrides this, `rev`
      # alone is not enough for troubleshooting; we need to know the whole expression.
      rev = flake.inputs.self.rev or "";
    };
}
