module Handler.NewRanking where

import Import

getNewRankingR :: CubeId -> Handler Html
getNewRankingR cubeId = do
    uid <- requireAuthId
    rid <- runDB $ do
        mrid <- getBy $ UniqueRanking uid cubeId
        case mrid of
            Nothing -> insert $ Ranking uid cubeId
            Just (Entity rid _) -> return rid
    redirect (ViewRankingR rid)
