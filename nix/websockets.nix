{ mkDerivation
, async
, attoparsec
, base
, base64-bytestring
, binary
, bytestring
, bytestring-builder
, case-insensitive
, containers
, criterion
, entropy
, HUnit
, network
, QuickCheck
, random
, SHA
, stdenv
, streaming-commons
, test-framework
, test-framework-hunit
, test-framework-quickcheck2
, text
}:
mkDerivation {
  pname = "websockets";
  version = "0.12.6.1";
  sha256 = "7ddb936d5fb003ecd41b89d90d0f134f1f474e6f9b8461d236b9c2c1413ae3ee";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async
    attoparsec
    base
    base64-bytestring
    binary
    bytestring
    bytestring-builder
    case-insensitive
    containers
    entropy
    network
    random
    SHA
    streaming-commons
    text
  ];
  testHaskellDepends = [
    async
    attoparsec
    base
    base64-bytestring
    binary
    bytestring
    bytestring-builder
    case-insensitive
    containers
    entropy
    HUnit
    network
    QuickCheck
    random
    SHA
    streaming-commons
    test-framework
    test-framework-hunit
    test-framework-quickcheck2
    text
  ];
  benchmarkHaskellDepends = [
    async
    attoparsec
    base
    base64-bytestring
    binary
    bytestring
    bytestring-builder
    case-insensitive
    containers
    criterion
    entropy
    network
    random
    SHA
    text
  ];
  doCheck = false;
  homepage = "http://jaspervdj.be/websockets";
  description = "A sensible and clean way to write WebSocket-capable servers in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
