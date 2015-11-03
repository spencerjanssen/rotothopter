module Handler.NewDraftInvite where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Common (bootstrapLabel)

getNewDraftInviteR :: Handler Html
getNewDraftInviteR = do
    uid <- requireAuthId
    (formWidget, formEnctype) <- generateFormPost $ inviteForm uid
    defaultLayout $ do
        setTitle "Create a new draft invitation"
        $(widgetFile "new-draft-invite")

postNewDraftInviteR :: Handler Html
postNewDraftInviteR = do
    uid <- requireAuthId
    ((FormSuccess newInvite, _), _) <- runFormPost $ inviteForm uid
    t <- liftIO getCurrentTime
    h <- newInviteHash
    _invid <- runDB $ insert $ newInvite h t
    -- todo, redirect to draft join page
    redirect (ViewDraftInviteR h)

inviteForm :: UserId -> Form (InviteHash -> UTCTime -> DraftInvite)
inviteForm uid = renderBootstrap3 BootstrapBasicForm $ mk
    <$> (entityKey <$> areq cubeField (bootstrapLabel "Cube Name") Nothing)
    <*> areq intField (bootstrapLabel "Rounds") (Just 45)
 where
    mk cid rds = DraftInvite uid cid rds
    cubeField = checkMMap findCube (view cubeName . entityVal) textField
    findCube txt =
        maybe (Left txt) Right <$> runDB (getBy $ UniqueCubeName txt)
