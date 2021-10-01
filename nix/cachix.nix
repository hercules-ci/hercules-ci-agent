{ mkDerivation
, async
, base
, base64-bytestring
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
, lib
, lzma-conduit
, megaparsec
, memory
, mmorph
, netrc
, nix
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
  version = "0.6.1";
  sha256 = "e7dab0290f90338cffbce0275ea7d74ff103e8709882b9f6a08f42550638f539";
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
  libraryPkgconfigDepends = [ nix ];
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
