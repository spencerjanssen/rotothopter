module Handler.CubeList where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Data.Text (strip)
import qualified Database.Esqueleto as E

getNewCubeListR :: Handler Html
getNewCubeListR = do
    void requireAuth
    (formWidget, formEnctype) <- generateFormPost cubeForm
    defaultLayout $ do
        setTitle "Post your cube list"
        $(widgetFile "post-cubelist")

postNewCubeListR :: Handler Html
postNewCubeListR = do
    uid <- requireAuthId
    ((FormSuccess (name, cards), _), _) <- runFormPost cubeForm
    cid <- runDB $ do
        cid <- insert $ Cube uid name
        mapM_ (void . insert . CubeCard cid) cards
        return cid
    redirect (ViewCubeListR cid)

cubeForm :: Form (Text, [Text])
cubeForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq textField "Cube name" Nothing
    <*> ((map strip . lines . unTextarea) <$> areq textareaField "The cube list" Nothing)

getViewCubeListR :: CubeId -> Handler Html
getViewCubeListR cid = do
    (cname, cs) <- runDB $ do
        Just (Cube _ cname) <- get cid
        cs <- E.select $ E.from $ \(cubeCard `E.LeftOuterJoin` card) -> do
            E.on $ E.just (cubeCard E.^. CubeCardName) E.==. card E.?. CardName
            E.where_ (E.val cid E.==. cubeCard E.^. CubeCardCube)
            return (cubeCard E.^. CubeCardName, card)
        return (cname, cs)
    defaultLayout $ do
        setTitle "View Cube List"
        $(widgetFile "view-cubelist")
