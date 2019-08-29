{ mkDerivation
, ansi-wl-pprint
, base
, bytestring
, containers
, fetchgit
, hashable
, hspec
, mtl
, parsec
, parsers
, QuickCheck
, raw-strings-qq
, regex-posix
, stdenv
, template-haskell
, transformers
, unordered-containers
, vector
}:
mkDerivation {
  pname = "inline-c";
  version = "0.8.0.1";
  src = fetchgit {
    url = "https://github.com/hercules-ci/inline-c";
    sha256 = "080wp413dbdwanh53hcg0h2ni75l3f0zcmdfdpah7dq9rldiqmzg";
    rev = "18cdcb008d070931748de1b81d7b84eb8a492a8c";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/inline-c; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint
    base
    bytestring
    containers
    hashable
    mtl
    parsec
    parsers
    template-haskell
    transformers
    unordered-containers
    vector
  ];
  testHaskellDepends = [
    ansi-wl-pprint
    base
    containers
    hashable
    hspec
    parsers
    QuickCheck
    raw-strings-qq
    regex-posix
    template-haskell
    transformers
    unordered-containers
    vector
  ];
  description = "Write Haskell source files including C code inline. No FFI required.";
  license = stdenv.lib.licenses.mit;
}
