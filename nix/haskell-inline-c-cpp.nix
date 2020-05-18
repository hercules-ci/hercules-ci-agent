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
  version = "0.4.0.2";
  sha256 = "f7c6a6fb1c22784b9592461e4ffcad964665a9b130f824f3f176c943b0b06759";
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
