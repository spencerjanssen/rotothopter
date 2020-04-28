{ }:
let pkgs = import (import ./nix/sources.nix).nixpkgs {} ;
    oldPkgs = import (import ./nix/sources.nix).old-nixpkgs {} ;
    roto = f: pkgs.haskell.lib.overrideCabal (import ./default.nix {}) f;
    pspkgs = oldPkgs.haskell.packages.ghc802.override {
        overrides = self: super: {
            purescript = (self.callPackage ./purescript-0.10.7.nix {}).overrideScope (self: super: {
                aeson = self.aeson_0_11_3_0;
                http-client = self.http-client_0_4_31_2;
                http-client-tls = self.http-client-tls_0_2_4_1;
                pipes = self.pipes_4_2_0;
                websockets = self.websockets_0_9_8_2;
                });
        };
    };
    bower_components = oldPkgs.buildBowerComponents {
        name = "rotothopter-purescript-frontend-bower";
        generated = ./purescript/bower-generated.nix;
        src = ./purescript;
    };
    ps_0_10_7 = pspkgs.purescript;
    frontend_assets = (import ./purescript/default.nix {pkgs = oldPkgs;}).package.overrideAttrs (oldAttrs: {
            # dontNpmInstall = true;
            buildInputs = oldAttrs.buildInputs ++ [ps_0_10_7 bower_components];
            preRebuild = ''
                cp -r ${bower_components}/bower_components .
            '';
            postInstall = ''
                cp static/dist/* $out
                rm -rf lib
            '';
        });
in
{
    frontend_assets = frontend_assets;
    rotothopter_static = roto (drv: {
        pname = "rotothopter_static";
        enableSharedExecutables = false;
        configureFlags = [ "-f executable-only" ];
        isLibrary = false;
        enableSeparateDataOutput = false;
        doCheck = false;
        patchPhase = ''
            mkdir -p static/gen
            rm static/gen/manifest.json
            rm static/gen/main-dummy.js
            cp ${frontend_assets}/*.js static/gen/
            cp ${frontend_assets}/manifest.json static/gen/
        '';
    });
    ps_0_10_7 = ps_0_10_7;
}
