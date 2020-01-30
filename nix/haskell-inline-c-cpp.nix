{ mkDerivation
, base
, containers
, hspec
, inline-c
, safe-exceptions
, stdenv
, template-haskell
, fetchgit
}:
mkDerivation {
  pname = "inline-c-cpp";
  version = "0.4.0.1";
  # Fetching from git only because of https://github.com/fpco/inline-c/pull/106
  src = fetchgit {
    url = "https://github.com/fpco/inline-c";
    sha256 = "1yn5l9962imc3qq1hzz1gvs49f4j0vnb5q3yf4lc3ssdxkf4nd2n";
    rev = "dc7f8aac5d689630b77a74d762e4e5a633322d8d";
    fetchSubmodules = true;
  } + "/inline-c-cpp";
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
