module Handler.ViewDraftInvite where

import Import
import qualified Database.Esqueleto as E

getViewDraftInviteR :: InviteHash -> Handler Html
getViewDraftInviteR inviteHash = do
    Entity invId inv <- runDB (getBy404 (UniqueInviteHash inviteHash))
    cube <- runDB $ get404 (inv ^. draftInviteCube)
    members <- draftInvitees invId
    mauth <- maybeAuth
    defaultLayout $ do
        setTitle "Draft Invite"
        $(widgetFile "view-invite")

draftInvitees :: DraftInviteId -> Handler [Entity User]
draftInvitees did = runDB query
 where
    query = E.select $ E.from $ \(invitee `E.InnerJoin` user) -> do
                E.on $ invitee E.^. DraftInviteeDrafter E.==. user E.^. UserId
                E.where_ $ invitee E.^. DraftInviteeDraftInvite E.==. E.val did
                return user
