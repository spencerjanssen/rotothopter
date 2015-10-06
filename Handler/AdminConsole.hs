module Handler.AdminConsole where

import Import

getAdminConsoleR :: Handler Html
getAdminConsoleR = do
    defaultLayout $ do
        setTitle "Admin Console"
        $(widgetFile "admin-console")
