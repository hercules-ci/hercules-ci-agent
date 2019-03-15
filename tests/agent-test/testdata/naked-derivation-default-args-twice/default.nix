{ name ? "myPackage" }:
{ foo = { name ? "myPackage" }: builtins.abort "Hold on, don't call me!";
  bar = derivation {
    name = name;
    builder = "foo";
    system = "x86_64-linux";
  };
}
