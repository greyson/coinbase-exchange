{ compiler ? "default", profiling ? false }:
let
  pkgs = (import <nixpkgs> {}).pkgs;
  hpkgs_base = (if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler});
  hpkgs_std = hpkgs_base.override {
      overrides = hself: hsuper: {
      };
    };
  hpkgs_profiled = hpkgs_std.override {
      override = hself: hsuper: {
        mkDerivation = args: hsuper.mkDerivation (args // {
          enableLibraryProfiling = true;
        });
      };
    };
  hpkgs = if profiling then hpkgs_profiled else hpkgs_std;
in (hpkgs.callPackage ./build.nix {}).env
