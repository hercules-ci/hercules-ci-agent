{
  inputs = {};
  outputs = { self, ... }: {
    herculesCI.onPush.default.outputs = {
      data-pkg = derivation {
        name = "data-pkg-1.0";
        builder = "foo";
        system = "x86_64-linux";
      };
      inherit (self) defaultPackage;
    };

    defaultPackage.x86_64-linux = derivation {
      name = "default-package";
      builder = "foo";
      system = "x86_64-linux";
    };
  };
}