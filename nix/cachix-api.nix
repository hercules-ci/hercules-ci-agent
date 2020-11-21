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
, hspec
, hspec-discover
, http-api-data
, http-media
, jose
, lens
, memory
, nix-narinfo
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
, time
, transformers
}:
mkDerivation {
  pname = "cachix-api";
  version = "0.5.0";
  sha256 = "9eeb6b8403f702a5983d558b29b67788a7bab9a09518b8dcec7f9ecdcddb6491";
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
    nix-narinfo
    protolude
    resourcet
    servant
    servant-auth
    servant-auth-server
    servant-client
    string-conv
    swagger2
    text
    time
    transformers
  ];
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
