module TestImport
    ( module TestImport
    , module X
    ) where

import qualified Prelude (id)
import Application           (makeFoundation)
import ClassyPrelude         as X hiding
    ( Index
    , index
    , uncons
    , unsnoc
    , cons
    , snoc
    , (<.>)
    )
import Database.Persist      as X hiding (get, (<.))
import Database.Persist.Sql  (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Foundation            as X
import Model                 as X
import Test.Hspec            as X
import Text.Shakespeare.Text (st)
import Yesod.Default.Config2 (ignoreEnv, loadAppSettings)
import Yesod.Test            as X
import Control.Lens as X

-- Wiping the database
import Database.Persist.Sqlite              (sqlDatabase, wrapConnection, createSqlPool)
import qualified Database.Sqlite as Sqlite
import Control.Monad.Logger                 (runLoggingT)
import Settings (appDatabaseConf, appRoot, SqlBackendConf(Sqlite))
import Yesod.Core (messageLoggerSource)

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    pool <- fmap appConnPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

withApp :: SpecWith (App, a -> a) -> Spec
withApp = before $ do
    settings <- loadAppSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    return (foundation, Prelude.id)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = do
    -- In order to wipe the database, we need to temporarily disable foreign key checks.
    -- Unfortunately, disabling FK checks in a transaction is a noop in SQLite.
    -- Normal Persistent functions will wrap your SQL in a transaction,
    -- so we create a raw SQLite connection to disable foreign keys.
    -- Foreign key checks are per-connection, so this won't effect queries outside this function.

    -- Aside: SQLite by default *does not enable foreign key checks*
    -- (disabling foreign keys is only necessary for those who specifically enable them).
    let settings = appSettings app
    Sqlite dbconf <- return $ appDatabaseConf settings
    sqliteConn <- rawConnection (sqlDatabase $ dbconf)
    disableForeignKeys sqliteConn

    let logFunc = messageLoggerSource app (appLogger app)
    pool <- runLoggingT (createSqlPool (wrapConnection sqliteConn) 1) logFunc

    flip runSqlPersistMPool pool $ do
        tables <- getTables
        sqlBackend <- ask
        let queries = map (\t -> "DELETE FROM " ++ connEscapeName sqlBackend (DBName t)) tables
        forM_ queries (\q -> rawExecute q [])

rawConnection :: Text -> IO Sqlite.Connection
rawConnection = Sqlite.open

disableForeignKeys :: Sqlite.Connection -> IO ()
disableForeignKeys conn = Sqlite.prepare conn "PRAGMA foreign_keys = OFF;" >>= void . Sqlite.step

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql "SELECT name FROM sqlite_master WHERE type = 'table';" []
    return (fmap unSingle tables)

authenticateAs email = do
    root <- appRoot . appSettings  <$> getTestYesod
    request $ do
        setMethod "POST"
        addPostParam "ident" email
        setUrl $ root ++ "/auth/page/dummy"

authenticateAdmin = do
    runDB $ insert $ User ident True Nothing
    authenticateAs ident
 where ident = "admin@test.com"

authenticateA = authenticateAs "a@test.com"

checkFailsNonAuth route =
    it "redirects for user that is not logged in" $ do
        get route
        statusIs 303

checkRequiresAuth route = do
    checkFailsNonAuth route
    it "succeeds for regular user" $ do
        authenticateA
        get route
        statusIs 200

checkRequiresAdmin route = do
    checkFailsNonAuth route
    it "fails for non-admin" $ do
        authenticateA
        get route
        statusIs 403
    it "succeeds for admin" $ do
        authenticateAdmin
        get route
        statusIs 200

testCubeName = "Test Cube"
testCubeList = ["Life", "Death" , "Life // Death", "Lightning Bolt"]

postCube name list = do
    authenticateA
    get NewCubeListR
    request $ do
        addToken
        byLabel "Cube name" name
        byLabel "The cube list" (unlines list)
        setMethod "POST"
        setUrl NewCubeListR

postTestCube = postCube testCubeName testCubeList

testLargeCubeName :: Text
testLargeCubeName = "Large Cube"

testLargeCube :: [Text]
testLargeCube = ["Card_" ++  pack (show i) | i <- [1 :: Int .. 360]]

testParticipants :: [Text]
testParticipants = [cons c "@test.com" | c <- take 6 ['a' ..]]

testDraftRounds :: Int
testDraftRounds = 5

postDraft cube participants rounds = do
    mapM_ authenticateAs participants

    authenticateA
    get NewDraftR
    request $ do
        addToken
        byLabel "Cube Name" cube
        byLabel "Participants" (unlines participants)
        byLabel "Rounds" (pack $ show rounds)
        setMethod "POST"
        setUrl NewDraftR

getOnlyDraftId = do
    Just (Entity did _) <- runDB $ selectFirst ([] :: [Filter Draft]) []
    return did

postDraftInvite cube = do
    authenticateA
    get (NewDraftInviteR cube)
    request $ do
        addToken
        byLabel "Rounds" "45"
        setMethod "POST"
        setUrl (NewDraftInviteR cube)
