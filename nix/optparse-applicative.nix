{ mkDerivation
, ansi-wl-pprint
, base
, bytestring
, fetchgit
, process
, QuickCheck
, stdenv
, transformers
, transformers-compat
}:
mkDerivation {
  pname = "optparse-applicative";
  version = "0.16.0.0";
  src = fetchgit {
    url = "https://github.com/hercules-ci/optparse-applicative.git";
    sha256 = "1pdirlh9i9glkmhpdxrlq8jcs09676bydfyigx2d73xcglqq80cn";
    rev = "c4d558158dcf97bb39b409c24f519b2358c64b6f";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    ansi-wl-pprint
    base
    process
    transformers
    transformers-compat
  ];
  testHaskellDepends = [ base bytestring QuickCheck ];
  homepage = "https://github.com/pcapriotti/optparse-applicative";
  description = "Utilities and combinators for parsing command line options";
  license = stdenv.lib.licenses.bsd3;
}
