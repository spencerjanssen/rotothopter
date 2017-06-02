module Handler.ViewRanking where

import Import

getViewRankingR :: RankingId -> Handler Html
getViewRankingR _ = do
    defaultLayout $ do
        setTitle "Ranking"
        $(widgetFile "view-ranking")
