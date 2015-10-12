{ compiler ? "ghc7102" }:
let pkgs = import <nixpkgs> {  };
in pkgs.pkgs.haskell.packages.${compiler}.callPackage ./rotothopter.nix { }
