{ mkDerivation
, aeson
, base
, bytestring
, containers
, cookie
, exceptions
, hashable
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
  pname = "hercules-ci-api";
  version = "0.4.0.0";
  src = ./hercules-ci-api;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    base
    bytestring
    containers
    cookie
    exceptions
    hashable
    http-api-data
    http-media
    lens
    lens-aeson
    memory
    network-uri
    profunctors
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
  executableHaskellDepends = [
    aeson
    base
    bytestring
    containers
    cookie
    exceptions
    hashable
    http-api-data
    http-media
    lens
    memory
    network-uri
    profunctors
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
