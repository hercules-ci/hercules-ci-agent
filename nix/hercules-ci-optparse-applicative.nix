{ mkDerivation
, base
, fetchgit
, lib
, prettyprinter
, prettyprinter-ansi-terminal
, process
, QuickCheck
, text
, transformers
, transformers-compat
}:
mkDerivation {
  pname = "hercules-ci-optparse-applicative";
  version = "0.18.1.0";
  src = fetchgit {
    url = "https://github.com/hercules-ci/optparse-applicative";
    sha256 = "1cgxc80zfgzk4rrhspnlj7790jb0ddq7ybj7qjan5xmjjir90763";
    rev = "4b376562b51ee58b27171c3316d2d5ddac108deb";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base
    prettyprinter
    prettyprinter-ansi-terminal
    process
    text
    transformers
    transformers-compat
  ];
  testHaskellDepends = [ base QuickCheck ];
  homepage = "https://github.com/hercules-ci/optparse-applicative";
  description = "Utilities and combinators for parsing command line options (fork)";
  license = lib.licenses.bsd3;
}
