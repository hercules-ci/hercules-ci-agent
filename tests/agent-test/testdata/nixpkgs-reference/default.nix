{
  herculesCI.onPush.default = {
    inputs = abort "bypassed in test";
    outputs = { nixpkgs ? null }: {
      inherit (import nixpkgs {}) hello;
    };
  };
}
