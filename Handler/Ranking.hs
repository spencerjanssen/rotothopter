module Handler.Ranking where

import Import
import Data.Aeson.TH (deriveJSON, defaultOptions)

data GetRankingReturn = GetRankingReturn
    { cards :: [Key Card]
    , rankings :: [RankingChoice] }
$(deriveJSON defaultOptions ''GetRankingReturn)

getRankingR :: RankingId -> Handler Value
getRankingR rankingId = do
    (Ranking _ cid) <- runDB $ get404 rankingId
    cs <- runDB $ map (view cubeEntryCard . entityVal) <$> selectList [CubeEntryCube ==. cid] []
    rs <- runDB $ map entityVal <$> selectList [RankingChoiceRanking ==. rankingId] []
    print ("asdf" :: String)
    returnJson $ GetRankingReturn cs rs

deleteRankingR :: RankingId -> Handler ()
deleteRankingR rankingId = do
    (Ranking uid' _) <- runDB $ get404 rankingId
    uid <- requireAuthId
    if uid == uid'
        then runDB $ delete rankingId
        else fail "not authorized"
