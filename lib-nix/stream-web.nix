{ mkDerivation, aeson, base, bytestring, containers, errors, extra, http-client,  http-types, network, stdenv, streamly, warp , parsec , attoparsec, mtl, fetchgit
}:
mkDerivation {
  pname = "stream-web";
  version = "0.0.1";
  src = fetchgit {
    url = "https://github.com/juspay/stream-web.git";
    rev = "ce6138e17260cad816c8d16685f5eef45d08a317";
    sha256 = "1l50yr9gf385grj3v3z11gblas9bbv6zsafpx0ldppg9bvdsdnfp";
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
