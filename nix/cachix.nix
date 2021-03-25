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
    sha256 = "148i1fv8fsfbnp5r307l087dj98vhwk2nalq75zx63kkn3mi2a4f";
    rev = "e149113d5bed0204c94a232a52fade96d1d7f29a";
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
