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
  version = "0.2.1";
  src = fetchgit {
    url = "https://github.com/cachix/cachix";
    sha256 = "1jixbikikw39ykplb9l7ak1qv00x477cgiswh6c9mb6xwj5pgnkj";
    rev = "f55bc800f941cb8fe78d61ebf9ed99d8f0aa5404";
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
