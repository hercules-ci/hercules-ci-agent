{ mkDerivation
, attoparsec
, base
, containers
, filepath
, hspec
, QuickCheck
, stdenv
, text
}:
mkDerivation {
  pname = "nix-narinfo";
  version = "0.1.0.1";
  sha256 = "900dab2ab230e291915f1a11f3b38c9e44f658c1988f713c0a92c1c53de4852f";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ attoparsec base containers text ];
  executableHaskellDepends = [ attoparsec base text ];
  testHaskellDepends = [
    attoparsec
    base
    filepath
    hspec
    QuickCheck
    text
  ];
  homepage = "https://github.com/sorki/nix-narinfo";
  description = "Parse and render .narinfo files";
  license = stdenv.lib.licenses.bsd3;
}
