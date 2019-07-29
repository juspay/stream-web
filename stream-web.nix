{ mkDerivation, aeson, base, bytestring, containers, errors, extra, http-client,  http-types, network, stdenv, streamly, warp , parsec , attoparsec
}:
mkDerivation {
  pname = "stream-web";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers errors extra http-client http-types network streamly warp parsec attoparsec
  ];
  description = "Web Server";
  license = "BSD3";
  hydraPlatforms = stdenv.lib.platforms.none;
}
