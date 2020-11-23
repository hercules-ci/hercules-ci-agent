{ mkDerivation, ansi-wl-pprint, base, bytestring, containers
, fetchgit, hashable, hspec, mtl, parsec, parsers, QuickCheck
, raw-strings-qq, regex-posix, split, stdenv, template-haskell
, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "inline-c";
  version = "0.9.1.3";
  src = fetchgit {
    url = "git://github.com/hercules-ci/inline-c.git";
    sha256 = "1byhli8gnjzmy00igwz8n3ixg3s6gmdhps0f559hqsx38rpfy8ya";
    rev = "196c65b7113cced2be031aefea4a1854f27037f1";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/inline-c; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base bytestring containers hashable mtl parsec
    parsers template-haskell transformers unordered-containers vector
  ];
  testHaskellDepends = [
    ansi-wl-pprint base containers hashable hspec parsers QuickCheck
    raw-strings-qq regex-posix split template-haskell transformers
    unordered-containers vector
  ];
  description = "Write Haskell source files including C code inline. No FFI required.";
  license = stdenv.lib.licenses.mit;
}
