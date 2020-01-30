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
, jose
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
  version = "0.4.0";
  src = fetchgit {
    url = "https://github.com/cachix/cachix";
    sha256 = "1ppsndzsav572kbjh0prkri3vvgsgxznkyzp3k4l28f9vsrbx280";
    rev = "ba89b84c4de1acac8db86023ddf888a54c204fa8";
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
    jose
    lens
    memory
    protolude
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
  executableHaskellDepends = [ aeson base protolude ];
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
