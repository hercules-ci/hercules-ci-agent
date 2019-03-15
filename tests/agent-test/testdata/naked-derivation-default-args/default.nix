{ name ? "myPackage" }:
derivation {
  name = name;
  builder = "foo";
  system = "x86_64-linux";
}
