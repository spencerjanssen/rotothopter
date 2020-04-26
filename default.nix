{ compiler ? "ghc865" }:
let pkgs = import (import ./nix/sources.nix).nixpkgs {};
in pkgs.haskell.packages.${compiler}.callPackage ./rotothopter.nix { }
