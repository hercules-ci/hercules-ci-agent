{ mkDerivation
, async
, base
, base64-bytestring
, boost
, bytestring
, cachix-api
, concurrent-extra
, conduit
, conduit-extra
, containers
, cookie
, cryptonite
, dhall
, directory
, ed25519
, fetchgit
, filepath
, fsnotify
, hercules-ci-cnix-store
, here
, hspec
, hspec-discover
, http-client
, http-client-tls
, http-conduit
, http-types
, inline-c
, inline-c-cpp
, lib
, lzma-conduit
, megaparsec
, memory
, mmorph
, netrc
, nixUnstable
, optparse-applicative
, process
, protolude
, resourcet
, retry
, safe-exceptions
, servant
, servant-auth
, servant-auth-client
, servant-client
, servant-client-core
, servant-conduit
, stm
, temporary
, text
, unix
, uri-bytestring
, vector
, versions
}:
mkDerivation {
  pname = "cachix";
  version = "0.6.0";
  src = fetchgit {
    url = "https://github.com/hercules-ci/cachix";
    sha256 = "07xnq20rrnr0c7kyq8ngg3dzd4s11i2gzmvzm7nmg9sqwjgr40j4";
    rev = "84a0a55359a58030ca604117faa8883ec8632009";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/cachix; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async
    base
    base64-bytestring
    bytestring
    cachix-api
    concurrent-extra
    conduit
    conduit-extra
    containers
    cookie
    cryptonite
    dhall
    directory
    ed25519
    filepath
    fsnotify
    hercules-ci-cnix-store
    here
    http-client
    http-client-tls
    http-conduit
    http-types
    inline-c
    inline-c-cpp
    lzma-conduit
    megaparsec
    memory
    mmorph
    netrc
    optparse-applicative
    process
    protolude
    resourcet
    retry
    safe-exceptions
    servant
    servant-auth
    servant-auth-client
    servant-client
    servant-client-core
    servant-conduit
    stm
    text
    unix
    uri-bytestring
    vector
    versions
  ];
  librarySystemDepends = [ boost ];
  libraryPkgconfigDepends = [ nixUnstable ];
  executableHaskellDepends = [ base cachix-api ];
  executableToolDepends = [ hspec-discover ];
  testHaskellDepends = [
    base
    cachix-api
    directory
    here
    hspec
    protolude
    servant-auth-client
    temporary
  ];
  homepage = "https://github.com/cachix/cachix#readme";
  description = "Command line client for Nix binary cache hosting https://cachix.org";
  license = lib.licenses.asl20;
}
