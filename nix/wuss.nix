{ mkDerivation
, base
, bytestring
, connection
, network
, stdenv
, websockets
}:
mkDerivation {
  pname = "wuss";
  version = "1.1.15";
  sha256 = "f80cc4ba0fb17d9df138a442c8f49883fff8bfc7410c5fa1ada4d1abaa4958c4";
  libraryHaskellDepends = [
    base
    bytestring
    connection
    network
    websockets
  ];
  homepage = "https://github.com/tfausak/wuss#readme";
  description = "Secure WebSocket (WSS) clients";
  license = stdenv.lib.licenses.mit;
}
