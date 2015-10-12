{ }:
let pkgs = import <nixpkgs> {  };
in
with import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; };
let roto = f: overrideCabal (import ./default.nix {}) f;
in
{
    rotothopter_dynamic = roto (drv: {});
    rotothopter_static = roto (drv: {
        pname = "rotothopter_static";
        enableSharedExecutables = false;
        configureFlags = [ "-f executable-only" ];
        isLibrary = false;
        doCheck = false;
    });
}
