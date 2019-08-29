{ mkDerivation
, aeson
, async
, base
, bytestring
, conduit
, conduit-extra
, containers
, cookie
, directory
, filepath
, hercules-ci-api
, hspec
, http-api-data
, mmorph
, protolude
, random
, servant
, servant-auth-server
, servant-conduit
, servant-server
, stdenv
, stm
, tar-conduit
, text
, uuid
, warp
}:
mkDerivation {
  pname = "hercules-ci-agent-test";
  version = "0.1.0.0";
  src = tests/agent-test;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson
    async
    base
    bytestring
    conduit
    conduit-extra
    containers
    cookie
    directory
    filepath
    hercules-ci-api
    hspec
    http-api-data
    mmorph
    protolude
    random
    servant
    servant-auth-server
    servant-conduit
    servant-server
    stm
    tar-conduit
    text
    uuid
    warp
  ];
  homepage = "https://github.com/hercules-ci/hercules-ci#readme";
  license = stdenv.lib.licenses.asl20;
}
