let inherit (import ./nix/package-set.nix { }) hsPkgs shell frontend_assets;
in
{
  inherit shell frontend_assets;
  rotothopter = hsPkgs.rotothopter.components.exes.rotothopter;
}
