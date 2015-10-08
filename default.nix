{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
let pkgs = import <nixpkgs> { };
in nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./rotothopter.nix { }
