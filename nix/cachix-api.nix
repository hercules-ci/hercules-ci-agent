{ mkDerivation
, aeson
, base
, base16-bytestring
, bytestring
, conduit
, cookie
, cryptonite
, deepseq
, exceptions
, fetchgit
, hspec
, hspec-discover
, http-api-data
, http-media
, lens
, memory
, protolude
, resourcet
, servant
, servant-auth
, servant-auth-server
, servant-auth-swagger
, servant-client
, servant-swagger
, servant-swagger-ui-core
, stdenv
, string-conv
, swagger2
, text
, transformers
}:
mkDerivation {
  pname = "cachix-api";
  version = "0.3.0";
  src = fetchgit {
    url = "https://github.com/cachix/cachix";
    sha256 = "1k9h6iaz1185r17safrncnlljg1qlaf71fpfzzz093g5073407j0";
    rev = "5c7272351c4e87adabfb8a31e5b9c1db2980abf3";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/cachix-api; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    base
    base16-bytestring
    bytestring
    conduit
    cookie
    cryptonite
    deepseq
    exceptions
    http-api-data
    http-media
    lens
    memory
    resourcet
    servant
    servant-auth
    servant-auth-server
    servant-auth-swagger
    servant-client
    servant-swagger
    string-conv
    swagger2
    text
    transformers
  ];
  executableHaskellDepends = [ aeson base ];
  testHaskellDepends = [
    aeson
    base
    base16-bytestring
    bytestring
    conduit
    cookie
    cryptonite
    hspec
    http-api-data
    http-media
    lens
    memory
    protolude
    servant
    servant-auth
    servant-auth-server
    servant-auth-swagger
    servant-swagger
    servant-swagger-ui-core
    string-conv
    swagger2
    text
    transformers
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/cachix/cachix#readme";
  description = "Servant HTTP API specification for https://cachix.org";
  license = stdenv.lib.licenses.asl20;
}
