{ mkDerivation, async, base, base16-bytestring, base64-bytestring
, bytestring, cachix-api, conduit, conduit-extra, cookie
, cryptonite, dhall, directory, ed25519, fetchgit, filepath
, fsnotify, here, hspec, hspec-discover, http-client
, http-client-tls, http-conduit, http-types, lzma-conduit
, megaparsec, memory, mmorph, netrc, optparse-applicative, process
, protolude, resourcet, retry, safe-exceptions, servant
, servant-auth, servant-auth-client, servant-client
, servant-client-core, servant-conduit, stdenv, temporary, text
, unix, uri-bytestring, versions
}:
mkDerivation {
  pname = "cachix";
  version = "0.2.0";
  src = fetchgit {
    url = "https://github.com/cachix/cachix";
    sha256 = "046r64n9z1k6l2hndcfz7wawpqxhrvvfr5lwfzq63rqrdf0ja38i";
    rev = "70a8673e15adf50833e5183cc4fd69cab35ba29d";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/cachix; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    async base base16-bytestring base64-bytestring bytestring
    cachix-api conduit conduit-extra cookie cryptonite dhall directory
    ed25519 filepath fsnotify here http-client http-client-tls
    http-conduit http-types lzma-conduit megaparsec memory mmorph netrc
    optparse-applicative process protolude resourcet retry
    safe-exceptions servant servant-auth servant-auth-client
    servant-client servant-client-core servant-conduit text unix
    uri-bytestring versions
  ];
  executableHaskellDepends = [ base cachix-api ];
  executableToolDepends = [ hspec-discover ];
  testHaskellDepends = [
    base cachix-api directory here hspec protolude temporary
  ];
  homepage = "https://github.com/cachix/cachix#readme";
  description = "Command line client for Nix binary cache hosting https://cachix.org";
  license = stdenv.lib.licenses.asl20;
}
