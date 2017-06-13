module Handler.CubeList where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Data.Text (strip)
import qualified Database.Esqueleto as E
import Common (bootstrapLabel, textAreaHeight)
import Handler.PrettyCard (cardListView)

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
        mapM_ (void . insert . CubeEntry cid . CardKey) cards
        return cid
    redirect (ViewCubeListR cid)

cubeForm :: Form (Text, [Text])
cubeForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq textField (bootstrapLabel "Cube name") Nothing
    <*> ((map strip . lines . unTextarea)
        <$> areq textareaField (textAreaHeight 20 $ bootstrapLabel "The cube list" ) Nothing)

getViewCubeListR :: CubeId -> Handler Html
getViewCubeListR cid = do
    muid <- maybeAuthId
    (cname, cs, mrid) <- runDB $ do
        Just (Cube _ cname) <- get cid
        cs <- fmap (map entityVal) $ E.select $ E.from $ \(cubeCard `E.InnerJoin` card) -> do
            E.on $ cubeCard E.^. CubeEntryCard E.==. card E.^. CardId
            E.where_ (E.val cid E.==. cubeCard E.^. CubeEntryCube)
            return card
        mrid <- join <$> traverse (\uid -> map entityKey <$> getBy (UniqueRanking uid cid)) muid
        return (cname, cs, mrid)
    defaultLayout $ do
        setTitle "View Cube List"
        addScript (StaticR js_jquery_hideseek_min_js)
        $(widgetFile "view-cubelist")
