{ mkDerivation, atomic-primops, base, containers, deepseq
, exceptions, fetchgit, gauge, ghc-prim, heaps, hspec
, lockfree-queue, monad-control, mtl, network, QuickCheck, random
, stdenv, transformers, transformers-base, typed-process
}:
mkDerivation {
  pname = "streamly";
  version = "0.6.1";
  src = fetchgit {
    url = "https://github.com/composewell/streamly.git";
    rev = "2f4f4cc1753a149e94e44a88695b0076368b3c37";
    sha256 = "11x9v1x1wipphsdm31xmggljh9srcj7hkdy4qp9k4zjasl5p2306";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    atomic-primops base containers deepseq exceptions ghc-prim heaps
    lockfree-queue monad-control mtl network transformers
    transformers-base
  ];
  testHaskellDepends = [
    base containers exceptions hspec mtl QuickCheck random transformers
  ];
  benchmarkHaskellDepends = [
    base deepseq gauge random typed-process
  ];
  homepage = "https://github.com/composewell/streamly";
  description = "Beautiful Streaming, Concurrent and Reactive Composition";
  license = stdenv.lib.licenses.bsd3;
  doCheck = false;
  doHaddock = true;
}
