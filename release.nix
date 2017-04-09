{ }:
let pkgs = import ./pinned-nixpkgs.nix ;
    roto = f: pkgs.haskell.lib.overrideCabal (import ./default.nix {}) f;
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
