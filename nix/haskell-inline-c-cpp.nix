{ mkDerivation
, base
, containers
, hspec
, inline-c
, safe-exceptions
, stdenv
, template-haskell
}:
mkDerivation {
  pname = "inline-c-cpp";
  version = "0.4.0.1";
  sha256 = "187b526453791f8c97be4daa4dc153c0b3c034df4815dcaa99f4cb8eb5a955af";
  libraryHaskellDepends = [
    base
    containers
    inline-c
    safe-exceptions
    template-haskell
  ];
  testHaskellDepends = [
    base
    containers
    hspec
    inline-c
    safe-exceptions
  ];
  description = "Lets you embed C++ code into Haskell";
  license = stdenv.lib.licenses.mit;
}
