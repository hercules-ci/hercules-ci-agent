{ mkDerivation
, async
, base
, base64-bytestring
, bytestring
, cachix-api
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
, here
, hnix-store-core
, hspec
, hspec-discover
, http-client
, http-client-tls
, http-conduit
, http-types
, inline-c
, inline-c-cpp
, lzma-conduit
, megaparsec
, memory
, mmorph
, netrc
, nix-main
, nix-store
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
, stdenv
, temporary
, text
, unix
, uri-bytestring
, versions
}:
mkDerivation {
  pname = "cachix";
  version = "0.2.1";
  src = fetchgit {
    url = "https://github.com/cachix/cachix";
    sha256 = "1jixbikikw39ykplb9l7ak1qv00x477cgiswh6c9mb6xwj5pgnkj";
    rev = "f55bc800f941cb8fe78d61ebf9ed99d8f0aa5404";
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
    here
    hnix-store-core
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
    text
    unix
    uri-bytestring
    versions
  ];
  libraryPkgconfigDepends = [ nix-main nix-store ];
  executableHaskellDepends = [ base cachix-api ];
  executableToolDepends = [ hspec-discover ];
  testHaskellDepends = [
    base
    cachix-api
    directory
    here
    hspec
    protolude
    temporary
  ];
  homepage = "https://github.com/cachix/cachix#readme";
  description = "Command line client for Nix binary cache hosting https://cachix.org";
  license = stdenv.lib.licenses.asl20;
}
