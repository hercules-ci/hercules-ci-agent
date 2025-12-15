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
  version = "0.19.0.0";
  src = fetchgit {
    url = "https://github.com/hercules-ci/optparse-applicative";
    hash = "sha256-cyqnj3U1IGXD2xS4sxQya3W3I3nbOMwW99kpIBPWGRk=";
    rev = "b55bb38a2aea0cf776aec707cdce7c7418146077";
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
