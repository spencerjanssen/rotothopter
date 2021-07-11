module Handler.UserProfile where

import Common (bootstrapLabel)
import Data.Text (strip)
import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

getUserProfileR :: Handler Html
getUserProfileR = do
    user <- requireUserInfo
    (form, _) <- generateFormPost $ userDetailsForm (user ^. userDisplayName)
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
userDetailsForm mt =
    renderBootstrap3
        BootstrapBasicForm
        (areq textField (bootstrapLabel "Your name") mt)
