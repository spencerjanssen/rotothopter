module Handler.LaunchDraftInvite where

import Prelude (reads)
import Import
import Database.Persist.Sql (toSqlKey)

postLaunchDraftInviteR :: InviteHash -> Handler Html
postLaunchDraftInviteR inviteHash = do
    uid <- requireAuthId
    Entity invId inv <- runDB (getBy404 (UniqueInviteHash inviteHash))
    when (inv ^. draftInviteCreator /= uid) $
        permissionDenied "You didn't create this draft"
    ps <- runDB $ selectList [DraftInviteeDraftInvite ==. invId] []
    when (null ps) $ fail "can't start a draft with 0 participants"
    ordering <- runInputPost $ participantForm $ map (view draftInviteeDrafter . entityVal) ps
    time <- liftIO getCurrentTime
    did <- runDB $ do
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
        forM_ (zip [0 ..] ordering) $ \(seat, invitee) ->
            void $ insert $ DraftParticipant invitee did seat
        return did
    redirect (ViewDraftR did)

participantForm :: [UserId] -> FormInput Handler [UserId]
participantForm allowedIds = ireq (multiParticipant allowedIds) "drafter"

multiParticipant :: [UserId] -> Field Handler [UserId]
multiParticipant allowedIds = Field
    { fieldParse = \rawVals _ -> do
        let es = do
                    uids <- mapM readEither rawVals
                    if sort uids == sort allowedIds
                        then return uids
                        else Left "User list does not match"
        return $ Just <$> es
    , fieldView = \_ _ _ _ _ -> [whamlet| |]
    , fieldEnctype = UrlEncoded }
 where
    readEither s = case reads $ unpack s of
        [(x, "")] -> return $ toSqlKey x
        _ -> Left $ fromString $ "can't parse user ID: " ++ show s
