let
  many = x: { recurseForDerivations = true;
              a = x; b = x; c = x; d = x; e = x;
              f = x; g = x; h = x; i = x; j = x;
            };
  pkg = derivation {
    name = "myPackage";
    builder = "foo";
    system = "x86_64-linux";
  };
in many (many (many (many (many (many pkg)))))
