module Handler.NewRanking where

import Import

getNewRankingR :: CubeId -> Handler Html
getNewRankingR cubeId = do
    uid <- requireAuthId
    rid <- runDB $ insert $ Ranking uid cubeId
    redirect (ViewRankingR rid)
