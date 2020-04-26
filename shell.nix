{ }:
let pkgs = import (import ./nix/sources.nix).nixpkgs {};
    roto = pkgs.haskell.lib.addBuildTools (import ./default.nix {}) [
        (import ./release.nix {}).ps_0_10_7
        ];
in roto.env
