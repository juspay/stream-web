{
  withHoogle ? true
}:
let
  /* inherit (import <nixpkgs> {}) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "19.03";
    sha256 = "0q2m2qhyga9yq29yz90ywgjbn9hdahs7i8wwlq7b55rdbyiwa5dy";
  }; */
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: rec {
          ghc =
            super.ghc // { withPackages = if withHoogle then super.ghc.withHoogle else super.ghc ; };
          ghcWithPackages =
            self.ghc.withPackages;
          # Haskell actually has a broken package called vision
          stream-web =
            self.callPackage ./stream-web.nix { };
          streamly =
            self.callPackage ./streamly.nix { };
        };
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };
  drv = pkgs.haskellPackages.stream-web;
in
  if pkgs.lib.inNixShell
    then
      drv.env.overrideAttrs(attrs:
        { buildInputs =
          with pkgs.haskellPackages;
          [
            cabal-install
            cabal2nix
            ghcid
            hindent
            hlint
            stylish-haskell
          ] ++ [ zlib ] ++ attrs.buildInputs;
        })
        else drv.overrideAttrs(attrs:
        {
          buildInputs = attrs.buildInputs ++ [ pkgs.zlib ];
        })
