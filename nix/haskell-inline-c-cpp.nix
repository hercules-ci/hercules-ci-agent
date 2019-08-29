{ mkDerivation
, base
, fetchgit
, hspec
, inline-c
, safe-exceptions
, stdenv
, template-haskell
}:
mkDerivation {
  pname = "inline-c-cpp";
  version = "0.4.0.0";
  src = fetchgit {
    url = "https://github.com/hercules-ci/inline-c";
    sha256 = "135z86c37b9k5v59vnicq6l1af6bdicyc8pnsq4ik2f2p9sqsksc";
    rev = "7ebf55ea582610fcb30fcbdebe13d5b301818168";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/inline-c-cpp; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base
    inline-c
    safe-exceptions
    template-haskell
  ];
  testHaskellDepends = [ base hspec inline-c safe-exceptions ];
  description = "Lets you embed C++ code into Haskell";
  license = stdenv.lib.licenses.mit;
}
