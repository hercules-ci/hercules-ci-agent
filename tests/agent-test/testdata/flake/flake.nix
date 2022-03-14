{
  inputs = {};
  outputs = { self, ... }: {
    packages.x86_64-linux.default = derivation {
      name = "default-package";
      builder = "foo";
      system = "x86_64-linux";
    };
  };
}