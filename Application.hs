{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application (
    getApplicationDev,
    appMain,
    develMain,
    makeFoundation,

    -- * for DevelMain
    getApplicationRepl,
    shutdownApp,

    -- * for GHCI
    handler,
    db,
#if DEVELOPMENT
    testUsers,
    seedTestUsers,
    mkAdmin,
    fillDraft,
#endif
) where

import Control.Monad.Logger (liftLoc, runLoggingT)
import Database.Persist.Postgresql (
    createPostgresqlPool,
    pgConnStr,
    pgPoolSize,
 )
import Database.Persist.Sqlite (
    createSqlitePool,
    runSqlPool,
    sqlDatabase,
    sqlPoolSize,
 )
import Import
import Language.Haskell.TH.Syntax (qLocation)
import Network.Wai.Handler.Warp (
    Settings,
    defaultSettings,
    defaultShouldDisplayException,
    getPort,
    runSettings,
    setHost,
    setOnException,
    setPort,
 )
import Network.Wai.Middleware.RequestLogger (
    Destination (Logger),
    IPAddrSource (..),
    OutputFormat (..),
    destination,
    mkRequestLogger,
    outputFormat,
 )
import System.Log.FastLogger (
    defaultBufSize,
    newStdoutLoggerSet,
    toLogStr,
 )

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!

import Handler.AdminAddUser
import Handler.AdminConsole
import Handler.AdminFeatureCube
import Handler.AllCubes
import Handler.AllDrafts
import Handler.Common
import Handler.CubeList
import Handler.Home
import Handler.JoinDraftInvite
import Handler.LaunchDraftInvite
import Handler.MakeDraftPick
import Handler.NewDraftInvite
import Handler.NewRanking
import Handler.PicksByParticipant
import Handler.Ranking
import Handler.RankingChoice
import Handler.UpdateMtgJson
import Handler.UserProfile
import Handler.ViewDraft
import Handler.ViewDraftInvite
import Handler.ViewRanking
import Handler.WatchDraft

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
            (appStaticDir appSettings)
    appDraftWatchers <- newTVarIO mempty

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App{..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ case appDatabaseConf appSettings of
        Sqlite c -> createSqlitePool (sqlDatabase c) (sqlPoolSize c)
        Postgres c -> createPostgresqlPool (pgConnStr c) (pgPoolSize c)

    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    -- Return the foundation
    return $ mkFoundation pool

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <-
        mkRequestLogger
            def
                { outputFormat =
                    if appDetailedRequestLogging $ appSettings foundation
                        then Detailed True
                        else
                            Apache
                                ( if appIpFromHeader $ appSettings foundation
                                    then FromFallback
                                    else FromSocket
                                )
                , destination = Logger $ loggerSet $ appLogger foundation
                }

    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
    setPort (appPort $ appSettings foundation) $
        setHost (appHost $ appSettings foundation) $
            setOnException
                ( \_req e ->
                    when (defaultShouldDisplayException e) $
                        messageLoggerSource
                            foundation
                            (appLogger foundation)
                            $(qLocation >>= liftLoc)
                            "yesod"
                            LevelError
                            (toLogStr $ "Exception from Warp: " ++ show e)
                )
                defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml, "config/secret.yml"] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <-
        loadYamlSettingsArgs
            -- fall back to compile-time values, set to [] to require values at runtime
            [configSettingsYmlValue]
            -- allow environment variables to override
            useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app

--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()

---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerFor App) a -> IO a
db = handler . runDB

#if DEVELOPMENT
testUsers :: [Text]
testUsers = [c ++ "@" ++ c ++ ".com" | c <- chars ]
 where
    chars = map singleton $ take 8 ['a' .. ]

seedTestUsers :: Handler [UserId]
seedTestUsers = runDB $ mapM (\u -> insert $ User u False Nothing) testUsers

mkAdmin :: Text -> Handler ()
mkAdmin u = runDB $ do
    Just (Entity i _) <- getBy (UniqueUser u)
    update i [UserAdmin =. True]
    return ()

fillDraft :: DraftId -> Handler ()
fillDraft did = do
    runDB $ do
        ps <- map (view draftParticipantDrafter . entityVal) <$> selectList [DraftParticipantDraft ==. did] []
        Just d <- get did
        t <- liftIO getCurrentTime
        let picks = fromIntegral (d ^. draftRounds) * length ps
            ucycle = ps ++ reverse ps ++ ucycle
        cs <- map entityVal <$> selectList [CubeEntryCube ==. d ^. draftCube] [Asc CubeEntryCard, LimitTo picks]
        forM_ (zip3 [0 ..] cs ucycle) $ \(i, c, u) -> void $ insert $ Pick did i (d ^. draftCube) (c ^. cubeEntryCard) u t
        return ()
#endif
