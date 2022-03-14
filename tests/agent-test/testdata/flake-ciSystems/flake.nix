{
  inputs = {};
  outputs = { self, ... }: {
    herculesCI.ciSystems = ["x86_64-linux" "aarch64-darwin"];
    packages.x86_64-linux.default = derivation {
      name = "default-package";
      builder = "foo";
      system = "x86_64-linux";
    };
  };
}