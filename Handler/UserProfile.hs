module Handler.UserProfile where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Data.Text (strip)

getUserProfileR :: Handler Html
getUserProfileR = do
    user <- requireUserInfo
    (form, _) <- generateFormPost $ userDetailsForm (userDisplayName user)
    defaultLayout $ do
        setTitle "User Profile"
        $(widgetFile "user-profile")

postUserProfileR :: Handler Html
postUserProfileR = do
    uid <- requireAuthId
    ((FormSuccess rawDisplayName, _), _) <- runFormPost $ userDetailsForm Nothing
    let trimDisplayName = strip rawDisplayName
    if null trimDisplayName
        then fail "your name can't be empty!"
        else do
            runDB $ update uid [UserDisplayName =. Just trimDisplayName]
            redirect UserProfileR

userDetailsForm :: Maybe Text -> Form Text
userDetailsForm mt = renderBootstrap3 BootstrapBasicForm (areq textField "Your name" mt)
