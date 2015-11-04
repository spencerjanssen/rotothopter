module Handler.LaunchDraftInvite where

import Import

postLaunchDraftInviteR :: InviteHash -> Handler Html
postLaunchDraftInviteR inviteHash = do
    uid <- requireAuthId
    Entity invId inv <- runDB (getBy404 (UniqueInviteHash inviteHash))
    when (inv ^. draftInviteCreator /= uid) $
        permissionDenied "You didn't create this draft"
    time <- liftIO getCurrentTime
    did <- runDB $ do
        ps <- selectList [DraftInviteeDraftInvite ==. invId] []
        when (null ps) $ fail "can't start a draft with 0 participants"
        deleteWhere [DraftInviteeDraftInvite ==. invId]
        delete invId
        did <- insert $ Draft
            (inv ^. draftInviteCreator)
            (inv ^. draftInviteCube)
            (fromIntegral $ length ps)
            (inv ^. draftInviteRounds)
            time
            inviteHash
        -- todo randomly shuffle or otherwise order the drafters
        forM_ (zip [0 ..] ps) $ \(seat, Entity _ invitee) ->
            void $ insert $ DraftParticipant
                                (invitee ^. draftInviteeDrafter)
                                did
                                seat
        return did
    redirect (ViewDraftR did)
