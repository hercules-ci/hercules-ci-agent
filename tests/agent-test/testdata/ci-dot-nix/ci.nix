let
  inherit (import ./.) you-can-import-this;
in
{
  "in-ci-dot-nix" = derivation {
    name = you-can-import-this;
    builder = "foo";
    system = "x86_64-linux";
  };
}