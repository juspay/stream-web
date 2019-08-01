{ mkDerivation, aeson, base, bytestring, containers, errors, extra, http-client,  http-types, network, stdenv, streamly, warp , parsec , attoparsec, mtl, fetchgit
}:
mkDerivation {
  pname = "stream-web";
  version = "0.0.1";
  src = fetchgit {
    url = "https://github.com/aravindgopall/stream-web.git";
    rev = "4b9d5fed4b252c63550db793935e57b77c0e0f84";
    sha256 = "0b3nn1ndjvyrgxbhz07b6qqpshh8caa6l7gsn5vx1avggd8x8fk4";
    fetchSubmodules = true;
  };
  isLibrary = true;
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
