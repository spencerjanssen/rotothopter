{ mkDerivation, aeson, base, bytestring, classy-prelude
, classy-prelude-conduit, classy-prelude-yesod, conduit, containers
, data-default, directory, esqueleto, fast-logger, file-embed
, hjsmin, hspec, http-conduit, lens, mime-mail, mime-mail-ses
, monad-control, monad-logger, persistent, persistent-postgresql
, persistent-sqlite, persistent-template, resourcet, safe
, shakespeare, stdenv, stm-delay, template-haskell, text, time
, transformers, unordered-containers, uuid, vector, wai-extra
, wai-logger, warp, yaml, yesod, yesod-auth, yesod-core
, yesod-eventsource, yesod-form, yesod-static, yesod-test
}:
mkDerivation {
  pname = "rotothopter";
  version = "0.0.4";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring classy-prelude classy-prelude-conduit
    classy-prelude-yesod conduit containers data-default directory
    esqueleto fast-logger file-embed hjsmin http-conduit lens mime-mail
    mime-mail-ses monad-control monad-logger persistent
    persistent-postgresql persistent-sqlite persistent-template safe
    shakespeare stm-delay template-haskell text time
    unordered-containers uuid vector wai-extra wai-logger warp yaml
    yesod yesod-auth yesod-core yesod-eventsource yesod-form
    yesod-static
  ];
  executableHaskellDepends = [
    aeson base bytestring classy-prelude classy-prelude-conduit
    classy-prelude-yesod conduit containers data-default directory
    esqueleto fast-logger file-embed hjsmin http-conduit lens mime-mail
    mime-mail-ses monad-control monad-logger persistent
    persistent-postgresql persistent-sqlite persistent-template safe
    shakespeare stm-delay template-haskell text time
    unordered-containers uuid vector wai-extra wai-logger warp yaml
    yesod yesod-auth yesod-core yesod-eventsource yesod-form
    yesod-static
  ];
  testHaskellDepends = [
    base classy-prelude classy-prelude-yesod esqueleto hspec lens
    monad-logger persistent persistent-sqlite resourcet shakespeare
    transformers yesod yesod-core yesod-test
  ];
  homepage = "https://github.com/spencerjanssen/rotothopter";
  license = stdenv.lib.licenses.bsd3;
}
