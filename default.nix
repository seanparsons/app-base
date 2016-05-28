{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, glue, lens, monad-control, mtl
      , postgresql-simple, stdenv, template-haskell, text, transformers
      , unordered-containers, wiring
      }:
      mkDerivation {
        pname = "app-base";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base glue lens monad-control mtl postgresql-simple template-haskell
          text transformers unordered-containers wiring
        ];
        testHaskellDepends = [ base ];
        homepage = "http://github.com/seanparsons/app-base#readme";
        description = "Basic scaffold code";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
