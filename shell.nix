{ }:
let pkgs = import ./pinned-nixpkgs.nix;
    roto = pkgs.haskell.lib.addBuildTools (import ./default.nix {}) [
        pkgs.haskell.packages.ghc802.yesod-bin
        pkgs.haskell.packages.ghc802.ghc-mod
        (import ./release.nix {}).ps_0_10_7
        ];
in roto.env
