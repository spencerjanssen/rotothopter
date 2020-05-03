module Handler.Home (getHomeR) where

import Import hiding (on, from, (^.), (==.), (||.), (<.), (>=.))
import Database.Esqueleto

getHomeR :: Handler Html
getHomeR = do
    muid <- maybeAuthId
    muserInfo <- traverse (runDB . loggedInInfo) muid
    featured <- runDB featuredCubes
    defaultLayout $ do
        setTitle "Welcome To rotothopter!"
        $(widgetFile "homepage")

data LoggedInInfo = LoggedInInfo
    { openDrafts :: [(DraftId, Text)]
    , completedDrafts :: [(DraftId, Text)]
    , draftInvites :: [(InviteHash, Text)]
    , rankings :: [(RankingId, Text)]
    }

loggedInInfo :: UserId -> ReaderT SqlBackend Handler LoggedInInfo
loggedInInfo userId = LoggedInInfo
    <$> userDrafts userId Open
    <*> userDrafts userId Completed
    <*> userInvites userId
    <*> userRankings userId

data DraftStatus = Open | Completed

userDrafts :: UserId -> DraftStatus -> ReaderT SqlBackend Handler [(DraftId, Text)]
userDrafts userId draftStatus = map munge <$> query
 where
    countCompare = case draftStatus of
        Open -> (<.)
        Completed -> (>=.)
    munge (draft, cube) = (entityKey draft, _cubeName $ entityVal cube)
    query = select $ from $ \(draft `InnerJoin` draftParticipant `InnerJoin` cube) -> do
        on $ draft ^. DraftCube ==. cube ^. CubeId
        on $ draft ^. DraftId ==. draftParticipant ^. DraftParticipantDraft
        let pickCount = subSelectCount $ from $ \pick -> do
                where_ $ (draft ^. DraftId) ==. pick ^. PickDraft
                return ()
            maxPicks = draft ^. DraftRounds *. draft ^. DraftParticipants
        where_ $ draftParticipant ^. DraftParticipantDrafter ==. val userId
            &&. pickCount `countCompare` maxPicks
        return (draft, cube)

userInvites :: UserId -> ReaderT SqlBackend Handler [(InviteHash, Text)]
userInvites userId = map munge <$> query
 where
    munge (draftInvite, cube) = (_draftInviteHash $ entityVal draftInvite, _cubeName $ entityVal cube)
    query = select $ distinct $ from $ \(draftInvite `InnerJoin` draftInvitee `InnerJoin` cube) -> do
        on $ draftInvite ^. DraftInviteCube ==. cube ^. CubeId
        on $ draftInvite ^. DraftInviteId ==. draftInvitee ^. DraftInviteeDraftInvite
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

featuredCubes :: ReaderT SqlBackend Handler [Entity Cube]
featuredCubes = select $ from $ \(featuredCube `InnerJoin` cube) -> do
    on $ featuredCube ^. FeaturedCubeCube ==. cube ^. CubeId
    orderBy [desc $ featuredCube ^. FeaturedCubeCreated]
    return cube
