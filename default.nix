{ compiler ? "ghc802" }:
let pkgs = import ./pinned-nixpkgs.nix ;
in pkgs.haskell.packages.${compiler}.callPackage ./rotothopter.nix { }
