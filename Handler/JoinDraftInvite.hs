module Handler.JoinDraftInvite where

import Import

postJoinDraftInviteR :: InviteHash -> Handler Html
postJoinDraftInviteR inviteHash = do
    uid <- requireAuthId
    Entity invId _ <- runDB (getBy404 (UniqueInviteHash inviteHash))
    -- TODO, check if user is already in the draft or just let the DB key constraints
    -- handle it?
    runDB $ void $ insert (DraftInvitee uid invId)
    redirect (ViewDraftInviteR inviteHash)
