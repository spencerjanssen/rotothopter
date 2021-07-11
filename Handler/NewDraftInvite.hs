module Handler.NewDraftInvite where

import Common (bootstrapLabel)
import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

getNewDraftInviteR :: CubeId -> Handler Html
getNewDraftInviteR cubeId = do
    uid <- requireAuthId
    (formWidget, formEnctype) <- generateFormPost $ inviteForm uid cubeId
    defaultLayout $ do
        setTitle "Create a new draft invitation"
        $(widgetFile "new-draft-invite")

postNewDraftInviteR :: CubeId -> Handler Html
postNewDraftInviteR cubeId = do
    uid <- requireAuthId
    ((FormSuccess newInvite, _), _) <- runFormPost $ inviteForm uid cubeId
    t <- liftIO getCurrentTime
    h <- newInviteHash
    _invid <- runDB $ insert $ newInvite h t
    -- todo, redirect to draft join page
    redirect (ViewDraftInviteR h)

inviteForm :: UserId -> CubeId -> Form (InviteHash -> UTCTime -> DraftInvite)
inviteForm uid cid =
    renderBootstrap3 BootstrapBasicForm $
        mk
            <$> areq intField (bootstrapLabel "Rounds") (Just 45)
  where
    mk rds = DraftInvite uid cid rds
