{ mkDerivation, aeson, base, bytestring, containers, deepseq, gauge
, hashable, hedgehog, hspec-megaparsec, htoml, htoml-megaparsec
, markdown-unlit, megaparsec, mtl, parsec, parser-combinators
, stdenv, tasty, tasty-discover, tasty-hedgehog, tasty-hspec
, tasty-silver, text, time, toml-parser, transformers
, unordered-containers
}:
mkDerivation {
  pname = "tomland";
  version = "1.0.1.0";
  sha256 = "a2735fa5bf60ca296dbf2fbb3e40377150a99d87b4d9fb7c91cba7dd570aa8b8";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers deepseq hashable megaparsec mtl
    parser-combinators text time transformers unordered-containers
  ];
  executableHaskellDepends = [ base text time unordered-containers ];
  executableToolDepends = [ markdown-unlit ];
  testHaskellDepends = [
    base bytestring containers hashable hedgehog hspec-megaparsec
    megaparsec tasty tasty-hedgehog tasty-hspec tasty-silver text time
    unordered-containers
  ];
  testToolDepends = [ tasty-discover ];
  benchmarkHaskellDepends = [
    aeson base deepseq gauge htoml htoml-megaparsec parsec text time
    toml-parser
  ];
  homepage = "https://github.com/kowainik/tomland";
  description = "Bidirectional TOML serialization";
  license = stdenv.lib.licenses.mpl20;
}