let
  hello = derivation {
    name = "myPackage";
    builder = "foo";
    system = "x86_64-linux";
  };
in
[
  hello
]
