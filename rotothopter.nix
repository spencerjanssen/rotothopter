{ mkDerivation, aeson, base, bytestring, classy-prelude
, classy-prelude-conduit, classy-prelude-yesod, conduit, containers
, data-default, directory, fast-logger, file-embed, hjsmin, hspec
, http-conduit, mime-mail, monad-control, monad-logger, persistent
, persistent-sqlite, persistent-template, resourcet, safe
, shakespeare, smtps-gmail, stdenv, template-haskell, text, time
, transformers, unordered-containers, vector, wai-extra, wai-logger
, warp, yaml, yesod, yesod-auth, yesod-core, yesod-form
, yesod-static, yesod-test
}:
mkDerivation {
  pname = "rotothopter";
  version = "0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring classy-prelude classy-prelude-conduit
    classy-prelude-yesod conduit containers data-default directory
    fast-logger file-embed hjsmin http-conduit mime-mail monad-control
    monad-logger persistent persistent-sqlite persistent-template safe
    shakespeare smtps-gmail template-haskell text time
    unordered-containers vector wai-extra wai-logger warp yaml yesod
    yesod-auth yesod-core yesod-form yesod-static
  ];
  executableHaskellDepends = [ base ];
  enableSharedExecutables = false;
  testHaskellDepends = [
    base classy-prelude classy-prelude-yesod hspec monad-logger
    persistent persistent-sqlite resourcet shakespeare transformers
    yesod yesod-core yesod-test
  ];
  doHaddock = false;
  doCheck = false;
  license = stdenv.lib.licenses.unfree;
}
