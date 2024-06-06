{ sources ? import ./sources.nix
, haskellCompiler ? "ghc882"
}:
let pkgs = import sources.nixpkgs (import sources."haskell.nix" {}).nixpkgsArgs;
    oldPkgs = import sources.old-nixpkgs {} ;
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
    ps_0_10_7 = pspkgs.purescript;
    bower_components = oldPkgs.buildBowerComponents {
        name = "rotothopter-purescript-frontend-bower";
        generated = ../purescript/bower-generated.nix;
        src = ../purescript;
    };
    frontend_assets = (import ../purescript/default.nix {pkgs = oldPkgs;}).package.overrideAttrs (oldAttrs: {
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
    nixIgnores = [
      "*.nix"
      "nix/"
      ".github/"
    ];
    addFrontendAssets = ''
      mkdir -p static/gen
      rm static/gen/manifest.json
      rm static/gen/main-dummy.js
      cp ${frontend_assets}/*.js static/gen/
      cp ${frontend_assets}/manifest.json static/gen/
    '';
    hsPkgs = pkgs.haskell-nix.cabalProject {
      src = pkgs.nix-gitignore.gitignoreSource nixIgnores ../.;
      ghc = pkgs.buildPackages.pkgs.haskell-nix.compiler.${haskellCompiler};
      name = "rotothopter";
      modules = [
        {
          packages.rotothopter.components.all.preBuild = pkgs.lib.mkForce addFrontendAssets;
          packages.rotothopter.components.library.preBuild = pkgs.lib.mkForce addFrontendAssets;
          packages.rotothopter.components.exes.rotothopter.preBuild = pkgs.lib.mkForce addFrontendAssets;
        }
      ];
    };
    nivPkgs = (import sources.niv {});
    hies = (import sources.all-hies {}).selection {
      selector = p: {
        ${haskellCompiler} = p.${haskellCompiler};
      };
    };
    shell = hsPkgs.shellFor {
      packages = ps: with ps; [
        rotothopter
      ];
      withHoogle = true;
      exactDeps = true;
      buildInputs = [
        nivPkgs.niv
        hies
        ps_0_10_7 
        pkgs.nixops
        pkgs.ghcid
      ];
    };
in
{
  inherit hsPkgs pkgs nivPkgs hies shell ps_0_10_7 bower_components frontend_assets;
}
