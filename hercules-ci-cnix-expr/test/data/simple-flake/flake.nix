{
  description = "A very simple flake";
  inputs = { };
  outputs = { ... }: {
    lib.greeting = "hello flake";
  };
}
