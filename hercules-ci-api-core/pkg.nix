{ mkDerivation
, aeson
, base
, bytestring
, containers
, cookie
, exceptions
, hashable
, http-api-data
, http-media
, katip
, lens
, lifted-base
, memory
, monad-control
, safe-exceptions
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
  pname = "hercules-ci-api-core";
  version = "0.1.0.0";
  src = ./hercules-ci-api-core;
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
    katip
    lens
    lifted-base
    memory
    monad-control
    safe-exceptions
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
