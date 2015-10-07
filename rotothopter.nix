{ mkDerivation, aeson, base, bytestring, classy-prelude
, classy-prelude-conduit, classy-prelude-yesod, conduit, containers
, data-default, directory, fast-logger, file-embed, hjsmin, hspec
, http-conduit, mime-mail, monad-control, monad-logger, persistent
, persistent-sqlite, persistent-template, resourcet, safe
, shakespeare, smtps-gmail, stdenv, template-haskell, text, time
, transformers, unordered-containers, vector, wai-extra, wai-logger
, warp, yaml, yesod, yesod-auth, yesod-core, yesod-eventsource
, yesod-form, yesod-static, yesod-test
, yesod-bin
}:
mkDerivation {
  pname = "rotothopter";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring classy-prelude classy-prelude-conduit
    classy-prelude-yesod conduit containers data-default directory
    fast-logger file-embed hjsmin http-conduit mime-mail monad-control
    monad-logger persistent persistent-sqlite persistent-template safe
    shakespeare smtps-gmail template-haskell text time
    unordered-containers vector wai-extra wai-logger warp yaml yesod
    yesod-auth yesod-core yesod-eventsource yesod-form yesod-static
    yesod-bin
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base classy-prelude classy-prelude-yesod hspec monad-logger
    persistent persistent-sqlite resourcet shakespeare transformers
    yesod yesod-core yesod-test
  ];
  doHaddock = false;
  doCheck = true;
  homepage = "https://github.com/spencerjanssen/rotothopter";
  license = stdenv.lib.licenses.bsd3;
}
