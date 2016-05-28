{ pkgs ? import <nixpkgs> {}, hspkgs ? (import <nixpkgs> {}).haskellPackages }:
pkgs.lib.overrideDerivation ((import ./default.nix) {})
(derivation: {
  buildInputs = derivation.buildInputs ++ [ hspkgs.stack hspkgs.ghcid ];
})
