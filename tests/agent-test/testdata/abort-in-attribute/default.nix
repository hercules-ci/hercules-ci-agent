{
  a-message-for-you = builtins.abort "I am not doing this today.";
  hello = derivation {
    name = "myPackage";
    builder = "foo";
    system = "x86_64-linux";
  };
}
