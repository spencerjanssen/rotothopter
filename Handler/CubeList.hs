module Handler.CubeList where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Data.Text (strip)

getNewCubeListR :: Handler Html
getNewCubeListR = do
    uid <- requireAuthId
    (formWidget, formEnctype) <- generateFormPost $ cubeForm uid
    defaultLayout $ do
        setTitle "Post your cube list"
        $(widgetFile "post-cubelist")

postNewCubeListR :: Handler Html
postNewCubeListR = do
    uid <- requireAuthId
    ((FormSuccess newCube, _), _) <- runFormPost $ cubeForm uid
    cid <- runDB $ insert newCube
    redirect (ViewCubeListR cid)

cubeForm :: UserId -> Form Cube
cubeForm uid = renderBootstrap3 BootstrapBasicForm $ Cube uid
    <$> areq textField "Cube name" Nothing
    <*> ((map strip . lines . unTextarea) <$> areq textareaField "The cube list" Nothing)

getViewCubeListR :: CubeId -> Handler Html
getViewCubeListR cid = do
    (cname, cs) <- runDB $ do
        Just (Cube _ cname ccards) <- get cid
        cs <- forM ccards $ \c -> (,) c <$> getBy (UniqueCardName c)
        return (cname, cs)
    defaultLayout $ do
        setTitle "View Cube List"
        $(widgetFile "view-cubelist")
