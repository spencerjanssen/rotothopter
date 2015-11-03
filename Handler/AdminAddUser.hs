module Handler.AdminAddUser where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Common (bootstrapLabel)

getAdminAddUserR :: Handler Html
getAdminAddUserR = do
    (formWidget, formEnctype) <- generateFormPost newUserForm
    defaultLayout $ do
        setTitle "Enter a new user"
        $(widgetFile "add-user")

postAdminAddUserR :: Handler Html
postAdminAddUserR = do
    ((FormSuccess (email, pseudo), _), _) <- runFormPost newUserForm
    _ <- runDB $ insert (User email False pseudo)
    redirect AdminAddUserR

newUserForm :: Form (Text, Maybe Text)
newUserForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq emailField (bootstrapLabel "Email") Nothing
    <*> aopt textField (bootstrapLabel "Name") Nothing
