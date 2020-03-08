{ mkDerivation
, aeson
, async
, attoparsec
, base
, base64-bytestring
, bdw-gc
, binary
, binary-conduit
, boost_context
, bytestring
, cachix
, cachix-api
, conduit
, conduit-extra
, containers
, directory
, exceptions
, filepath
, hercules-ci-api-agent
, hercules-ci-api-core
, hostname
, hspec
, http-client
, http-client-tls
, http-conduit
, inline-c
, inline-c-cpp
, katip
, lens
, lens-aeson
, lifted-async
, lifted-base
, monad-control
, mtl
, network
, nix-derivation
, nix-expr
, nix-main
, nix-store
, optparse-applicative
, process
, protolude
, safe-exceptions
, servant
, servant-auth-client
, servant-client
, servant-client-core
, stdenv
, stm
, system-filepath
, temporary
, text
, time
, tomland
, transformers
, transformers-base
, unix
, unliftio
, unliftio-core
, unordered-containers
, uuid
}:
mkDerivation {
  pname = "hercules-ci-agent";
  version = "0.6.5";
  src = ./hercules-ci-agent;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    async
    base
    binary
    binary-conduit
    bytestring
    conduit
    containers
    exceptions
    katip
    lifted-async
    lifted-base
    monad-control
    mtl
    optparse-applicative
    process
    protolude
    safe-exceptions
    stm
    text
    transformers-base
    unliftio
    unliftio-core
    uuid
  ];
  executableHaskellDepends = [
    aeson
    async
    attoparsec
    base
    base64-bytestring
    binary
    binary-conduit
    bytestring
    cachix
    cachix-api
    conduit
    conduit-extra
    containers
    directory
    exceptions
    filepath
    hercules-ci-api-agent
    hercules-ci-api-core
    hostname
    http-client
    http-client-tls
    http-conduit
    inline-c
    inline-c-cpp
    katip
    lens
    lens-aeson
    lifted-async
    lifted-base
    monad-control
    mtl
    network
    nix-derivation
    optparse-applicative
    process
    protolude
    safe-exceptions
    servant
    servant-auth-client
    servant-client
    servant-client-core
    stm
    system-filepath
    temporary
    text
    time
    tomland
    transformers
    transformers-base
    unix
    unliftio
    unliftio-core
    unordered-containers
    uuid
  ];
  executableSystemDepends = [ boost_context ];
  executablePkgconfigDepends = [
    bdw-gc
    nix-expr
    nix-main
    nix-store
  ];
  testHaskellDepends = [
    aeson
    async
    attoparsec
    base
    binary
    binary-conduit
    bytestring
    conduit
    containers
    exceptions
    hercules-ci-api-agent
    hercules-ci-api-core
    hspec
    katip
    lifted-async
    lifted-base
    monad-control
    nix-derivation
    optparse-applicative
    protolude
    safe-exceptions
    system-filepath
    text
    transformers-base
  ];
  doHaddock = false;
  homepage = "https://docs.hercules-ci.com";
  license = stdenv.lib.licenses.asl20;
}
