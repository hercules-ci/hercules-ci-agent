{ mkDerivation, base, containers, fetchgit, hspec, inline-c
, safe-exceptions, stdenv, template-haskell, vector
}:
mkDerivation {
  pname = "inline-c-cpp";
  version = "0.4.0.2";
  src = fetchgit {
    url = "git://github.com/hercules-ci/inline-c.git";
    sha256 = "1byhli8gnjzmy00igwz8n3ixg3s6gmdhps0f559hqsx38rpfy8ya";
    rev = "196c65b7113cced2be031aefea4a1854f27037f1";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/inline-c-cpp; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base containers inline-c safe-exceptions template-haskell
  ];
  testHaskellDepends = [
    base containers hspec inline-c safe-exceptions template-haskell
    vector
  ];
  description = "Lets you embed C++ code into Haskell";
  license = stdenv.lib.licenses.mit;
}
