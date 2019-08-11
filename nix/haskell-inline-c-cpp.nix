{ mkDerivation, base, fetchgit, hspec, inline-c, safe-exceptions
, stdenv, template-haskell
}:
mkDerivation {
  pname = "inline-c-cpp";
  version = "0.4.0.0";
  src = fetchgit {
    url = "https://github.com/hercules-ci/inline-c";
    sha256 = "080wp413dbdwanh53hcg0h2ni75l3f0zcmdfdpah7dq9rldiqmzg";
    rev = "18cdcb008d070931748de1b81d7b84eb8a492a8c";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/inline-c-cpp; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base inline-c safe-exceptions template-haskell
  ];
  testHaskellDepends = [ base hspec inline-c safe-exceptions ];
  description = "Lets you embed C++ code into Haskell";
  license = stdenv.lib.licenses.mit;
}
