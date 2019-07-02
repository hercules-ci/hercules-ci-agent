{ mkDerivation, async, base, base16-bytestring, base64-bytestring
, bytestring, cachix-api, conduit, conduit-extra, cookie
, cryptonite, dhall, directory, ed25519, filepath, fsnotify, here
, hspec, hspec-discover, http-client, http-client-tls, http-conduit
, http-types, lzma-conduit, megaparsec, memory, mmorph, netrc
, optparse-applicative, process, protolude, resourcet, retry
, safe-exceptions, servant, servant-auth, servant-auth-client
, servant-client, servant-client-core, servant-conduit, stdenv
, temporary, text, unix, uri-bytestring, versions
}:
mkDerivation {
  pname = "cachix";
  version = "0.2.0";
  src = /nix/store/irklvl0wgz2nl8sjdd7x3b884hfqqrfh-source/cachix;
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
