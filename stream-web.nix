{ mkDerivation, aeson, base, bytestring, containers, errors, extra, http-client,  http-types, network, stdenv, streamly, warp , parsec , attoparsec, mtl
}:
mkDerivation {
  pname = "stream-web";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring mtl network parsec streamly
  ];
  executableHaskellDepends = [
    aeson base bytestring containers errors extra http-client http-types network streamly warp parsec attoparsec
  ];
  description = "Web Server";
  license = "BSD3";
  hydraPlatforms = stdenv.lib.platforms.none;
}
