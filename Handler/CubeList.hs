module Handler.CubeList where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

getNewCubeListR :: Handler Html
getNewCubeListR = do
    (formWidget, formEnctype) <- generateFormPost cubeForm
    defaultLayout $ do
        setTitle "Post your cube list"
        $(widgetFile "post-cubelist")

postNewCubeListR :: Handler Html
postNewCubeListR = do
    ((FormSuccess newCube, _), _) <- runFormPost cubeForm
    cid <- runDB $ insert newCube
    redirect (ViewCubeListR cid)

cubeForm :: Form Cube
cubeForm = renderBootstrap3 BootstrapBasicForm $ Cube
    <$> areq textField "Cube name" Nothing
    <*> ((lines . unTextarea) <$> areq textareaField "The cube list" Nothing)

getViewCubeListR :: CubeId -> Handler Html
getViewCubeListR cid = do
    Just (Cube cname ccards) <- runDB $ get cid
    defaultLayout $ do
        setTitle "View Cube List"
        $(widgetFile "view-cubelist")
