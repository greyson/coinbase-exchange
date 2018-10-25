{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  hpkgs = if compiler == "default"
          then nixpkgs.pkgs.haskellPackages
          else nixpkgs.pkgs.haskell.packages.${compiler};
  callPackage = hpkgs.callPackage;
  dontCheck = nixpkgs.pkgs.haskell.lib.dontCheck;
in
dontCheck (callPackage ./build.nix {})
