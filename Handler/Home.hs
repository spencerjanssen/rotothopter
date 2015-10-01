module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Data.Text (strip)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    muser <- getUserInfo
    mwidge <- case muser of
        Nothing -> return Nothing
        Just user -> fmap (Just . fst) $ generateFormPost $ userDetailsForm (userDisplayName user)
    drafts <- runDB $ do
        ds <- selectList ([] :: [Filter Draft]) []
        mapM (\(Entity did d) -> do Just cu <- get (draftCubeId d); return (did, d, cu)) ds
    defaultLayout $ do
        setTitle "Welcome To rotothopter!"
        $(widgetFile "homepage")

postHomeR :: Handler ()
postHomeR = do
    uid <- requireAuthId
    ((FormSuccess rawDisplayName, _), _) <- runFormPost $ userDetailsForm Nothing
    let trimDisplayName = strip rawDisplayName
    if null trimDisplayName
        then fail "your name can't be empty!"
        else do
            runDB $ update uid [UserDisplayName =. Just trimDisplayName]
            redirect HomeR

userDetailsForm :: Maybe Text -> Form Text
userDetailsForm mt = renderBootstrap3 BootstrapBasicForm (areq textField "Your name" mt)
