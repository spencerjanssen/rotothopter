module Handler.RankingChoice where

import Import

postRankingChoiceR :: RankingId -> Text -> Text -> Handler ()
postRankingChoiceR rankingId better worse = do
    (Ranking uid' cid) <- runDB $ get404 rankingId
    runDB $ do
        void $ get404 (CubeEntryKey cid (CardKey better))
        void $ get404 (CubeEntryKey cid (CardKey worse))
    uid <- requireAuthId

    if uid == uid'
        then void $ runDB $ insert $ RankingChoice rankingId (CardKey better) (CardKey worse)
        else fail "not allowed"

deleteRankingChoiceR :: RankingId -> Text -> Text -> Handler ()
deleteRankingChoiceR rankingId better worse = do
    (Ranking uid' _) <- runDB $ get404 rankingId
    uid <- requireAuthId
    if uid == uid'
        then void $ runDB $ deleteBy $ UniqueRankingChoice rankingId (CardKey better) (CardKey worse)
        else fail "not allowed"
