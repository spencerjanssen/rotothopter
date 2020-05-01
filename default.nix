let inherit (import ./nix/package-set.nix { }) hsPkgs shell frontend_assets;
in
{
  inherit shell;
  rotothopter = hsPkgs.rotothopter.components.exes.rotothopter.overrideAttrs (attrs: {
    prePatch = attrs.prePatch + ''
      mkdir -p static/gen
      rm static/gen/manifest.json
      rm static/gen/main-dummy.js
      cp ${frontend_assets}/*.js static/gen/
      cp ${frontend_assets}/manifest.json static/gen/
    '';
  });
}
