{-# LANGUAGE NamedFieldPuns #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
-- import Text.Jasmine         (minifym)
import Yesod.Auth.Dummy     (authDummy)
import Yesod.Auth.OAuth2 (oauth2Url, getUserResponseJSON)
import Yesod.Auth.OAuth2.Google (oauth2GoogleScoped)
-- import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Control.Monad.Catch (throwM)
import Data.Aeson (withObject)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appDraftWatchers :: TVar (Map DraftId (TVar (Maybe Pick)))
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        sevenDaysInMinutes -- timeout in minutes
        "config/client_session_key.aes"
     where sevenDaysInMinutes = 7*24*60

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        muinfo <- getUserInfo
        mroute <- getCurrentRoute

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        nav <- widgetToPageContent $
            $(widgetFile "navbar")
        pc <- widgetToPageContent $ do
            addStylesheet (StaticR css_bootstrap_css)
            addScript (StaticR js_jquery_min_js)
            addScript (StaticR js_bootstrap_min_js)
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized FaviconAttnR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized AdminConsoleR _ = isAdmin
    isAuthorized UpdateMtgJsonR _ = isAdmin
    isAuthorized AdminAddUserR _ = isAdmin
    isAuthorized AdminFeatureCubeR {} _ = isAdmin
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent _ _ _ = return Nothing
        {-
    addStaticContent ext mime content = return Nothing
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs
        -}

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO app _source level = return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

requireUserInfo :: Handler User
requireUserInfo = do
    (Entity _ user) <- requireAuth
    return user

getUserInfo :: Handler (Maybe User)
getUserInfo = do
    mu <- maybeAuthId
    case mu of
        Nothing -> return Nothing
        Just uid -> runDB $ get uid

isAdmin :: Handler AuthResult
isAdmin = do
    muser <- getUserInfo
    return $ case view userAdmin <$> muser of
        Nothing -> AuthenticationRequired
        Just True -> Authorized
        Just False -> Unauthorized "you are not a site admin"

pseudonym :: User -> Text
pseudonym u = fromMaybe (u ^. userIdent) (u ^. userDisplayName)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

data GoogleUser = GoogleUser {googleUserEmail :: Text} deriving Generic

instance FromJSON GoogleUser where
    parseJSON = withObject "GoogleUserEmail" $ \o -> GoogleUser <$> o .: "email"

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    authenticate creds = do
        mident <- case credsPlugin creds of
            "google" -> return $ googleUserEmail <$> getUserResponseJSON creds
            _ -> return $ Right $ credsIdent creds
        case mident of
            Left msg -> do
                return $ ServerError $ pack $ show (msg, show creds)
            Right ident -> liftHandler $ runDB $ do
                x <- getBy $ UniqueUser ident
                uid <- case x of
                    Just (Entity uid _) -> return uid
                    Nothing -> insert $ User ident False Nothing
                return $ Authenticated uid

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins (App {appSettings}) = gauth ++ dauth
     where
        dauth | allowDummyAuth appSettings = [authDummy]
              | otherwise                  = []
        gauth = case (googleClientId appSettings, googleClientSecret appSettings) of
            (Just gci, Just gcs) -> [customGoogleAuth gci gcs]
            _ -> []

    authHttpManager = getHttpManager <$> getYesod

instance MonadFail Handler where
    fail = throwM . userError

customGoogleAuth :: Text -> Text -> AuthPlugin App
customGoogleAuth gci gcs = (oauth2GoogleScoped ["email", "profile"] gci gcs) { apLogin = loginFragment }
 where
    loginFragment tm = $(widgetFile "google-login")

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
