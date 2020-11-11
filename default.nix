(import ./nix/flake-compat.nix).defaultNix
  // {
  # Compatibility with pre-flake default.nix
  __functor = self: { system ? builtins.currentSystem }: self.packages.${system};
}
