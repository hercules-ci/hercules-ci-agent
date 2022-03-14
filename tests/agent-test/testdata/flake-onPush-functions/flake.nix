{
  inputs = { };
  outputs = { self, ... }: {
    herculesCI = { ref, ... }: {
      onPush.default.outputs = { ... }: {
        packages.x86_64-linux.default = derivation {
          name = "default-package";
          builder = "foo";
          system = "x86_64-linux";
        };
      };
      packages = throw "don't use flake.packages!";
    };
  };
}
