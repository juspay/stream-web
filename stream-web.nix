{ mkDerivation, aeson, base, bytestring, array, network, stdenv, streamly, attoparsec, mtl
}:
mkDerivation {
  pname = "stream-web";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring mtl network streamly array
  ];
  executableHaskellDepends = [
    aeson base bytestring network streamly mtl attoparsec array
  ];
  description = "Web Server";
  license = "BSD3";
  hydraPlatforms = stdenv.lib.platforms.none;
}
