{
  description = "Private inputs for working on hercules-ci-agent. These are used by the top level flake, but do not appear in consumers' lock files.";
  inputs = {
    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";
    nix-darwin.url = "github:LnL7/nix-darwin"; # test only
    # avoid duplicate nixpkgs in lock, and make sure we don't accidentally use it
    # bad UX though because attempting to use it will fail with a confusing error
    # "" means self.
    nix-darwin.inputs.nixpkgs.follows = "";
    pre-commit-hooks-nix.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks-nix.inputs.nixpkgs.follows = "";
  };

  # This flake is only used for its inputs.
  outputs = { ... }: { };
}
