{ mkDerivation
, aeson
, base
, bytestring
, containers
, cookie
, exceptions
, hashable
, hercules-ci-api-core
, hspec
, http-api-data
, http-media
, lens
, lens-aeson
, memory
, network-uri
, profunctors
, QuickCheck
, servant
, servant-auth
, servant-auth-swagger
, servant-swagger
, servant-swagger-ui-core
, stdenv
, string-conv
, swagger2
, text
, time
, uuid
}:
mkDerivation {
  pname = "hercules-ci-api-agent";
  version = "0.1.1.0";
  src = ./hercules-ci-api-agent;
  libraryHaskellDepends = [
    aeson
    base
    bytestring
    containers
    cookie
    exceptions
    hashable
    hercules-ci-api-core
    http-api-data
    http-media
    lens
    lens-aeson
    memory
    servant
    servant-auth
    servant-auth-swagger
    servant-swagger
    servant-swagger-ui-core
    string-conv
    swagger2
    text
    time
    uuid
  ];
  testHaskellDepends = [
    aeson
    base
    bytestring
    containers
    cookie
    exceptions
    hashable
    hspec
    http-api-data
    http-media
    lens
    memory
    network-uri
    profunctors
    QuickCheck
    servant
    servant-auth
    servant-auth-swagger
    servant-swagger
    servant-swagger-ui-core
    string-conv
    swagger2
    text
    time
    uuid
  ];
  homepage = "https://github.com/hercules-ci/hercules-ci-agent#readme";
  license = stdenv.lib.licenses.asl20;
}
