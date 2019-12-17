{ mkDerivation
, ansi-wl-pprint
, base
, bytestring
, containers
, hashable
, hspec
, mtl
, parsec
, parsers
, QuickCheck
, raw-strings-qq
, regex-posix
, split
, stdenv
, template-haskell
, transformers
, unordered-containers
, vector
}:
mkDerivation {
  pname = "inline-c";
  version = "0.9.0.0";
  sha256 = "061b089a087d7ce9c38b3f13c9c7046526c8651ebbd5dff889b0b31d14c3d320";
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
    split
    template-haskell
    transformers
    unordered-containers
    vector
  ];
  description = "Write Haskell source files including C code inline. No FFI required.";
  license = stdenv.lib.licenses.mit;
}
