module Handler.Home where

import Import hiding (on, from, (^.), (==.), (||.))
import Database.Esqueleto

getHomeR :: Handler Html
getHomeR = do
    muid <- maybeAuthId
    muserInfo <- traverse (runDB . loggedInInfo) muid
    defaultLayout $ do
        setTitle "Welcome To rotothopter!"
        $(widgetFile "homepage")

loggedInInfo :: UserId -> ReaderT SqlBackend Handler ([(DraftId, Text)], [(InviteHash, Text)], [(RankingId, Text)])
loggedInInfo userId = (,,) <$> userDrafts userId <*> userInvites userId <*> userRankings userId

userDrafts :: UserId -> ReaderT SqlBackend Handler [(DraftId, Text)]
userDrafts userId = map munge <$> query
 where
    munge (draft, cube) = (entityKey draft, _cubeName $ entityVal cube)
    query = select $ from $ \(draft `InnerJoin` draftParticipant `InnerJoin` cube) -> do
        on $ draft ^. DraftId ==. draftParticipant ^. DraftParticipantDraft
        on $ draft ^. DraftCube ==. cube ^. CubeId
        where_ $ draftParticipant ^. DraftParticipantDrafter ==. val userId
        return (draft, cube)

userInvites :: UserId -> ReaderT SqlBackend Handler [(InviteHash, Text)]
userInvites userId = map munge <$> query
 where
    munge (draftInvite, cube) = (_draftInviteHash $ entityVal draftInvite, _cubeName $ entityVal cube)
    query = select $ distinct $ from $ \(draftInvite `InnerJoin` draftInvitee `InnerJoin` cube) -> do
        on $ draftInvite ^. DraftInviteId ==. draftInvitee ^. DraftInviteeDraftInvite
        on $ draftInvite ^. DraftInviteCube ==. cube ^. CubeId
        where_ $ draftInvitee ^. DraftInviteeDrafter ==. val userId
            ||. draftInvite ^. DraftInviteCreator ==. val userId
        return (draftInvite, cube)

userRankings :: UserId -> ReaderT SqlBackend Handler [(RankingId, Text)]
userRankings userId = map munge <$> query
 where
    munge (ranking, cube) = (entityKey ranking, _cubeName $ entityVal cube)
    query = select $ from $ \(ranking `InnerJoin` cube) -> do
        on $ ranking ^. RankingCube ==. cube ^. CubeId
        where_ $ ranking ^. RankingPicker ==. val userId
        return (ranking, cube)
