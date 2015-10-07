module Handler.ViewDraft where

import Import
import Common
import Handler.PrettyCard
import Data.Time
import qualified Data.Map as Map

getViewDraftR :: DraftId -> Handler Html
getViewDraftR draftId = do
    Just draft <- runDB $ get draftId
    Just participants <- sequence <$> (runDB $ mapM get $ draftParticipants draft)
    Just (Cube cubename _) <- runDB $ get $ draftCubeId draft
    picks <- getDraftPicks draftId
    muid <- maybeAuthId
    allowedCards <- getPickAllowedCards draftId draft
    let pickmap = Map.fromList $ map (\p -> (draftPickPickNumber p, p)) picks
        lastRow = min (fromIntegral $ draftRounds draft-1) . fst
                . pickNumToRC draft $ Map.size pickmap
        mnextdrafter = getNextDrafter draft picks
        timediffByCell = Map.fromList $ do
            (p1, p2) <- zip picks (drop 1 picks)
            return ( draftPickPickNumber p2
                   , diffUTCTime (draftPickCreated p2) (draftPickCreated p1))
        timediffByCol c = sum $ do
            r <- [0 .. lastRow]
            Just d <- return $ Map.lookup (rcToPickNum draft (r, c)) timediffByCell
            return d
        (isNextDrafter, draftdone)
         = case (mnextdrafter, muid) of
                (Just nextdrafter, Just uid)
                    | uid == nextdrafter -> (True, False)
                (Nothing, _) -> (False, True)
                _ -> (False, False)
    catcards <- categorizeUnknownCardList allowedCards
    timestamp <- (elem "timestamp" . map fst . reqGetParams) <$> getRequest
    defaultLayout $ do
        setTitle $ if isNextDrafter
                    then "***Your turn to pick!"
                    else "View Cube Draft"
        addScriptRemote "http://timeago.yarp.com/jquery.timeago.js"
        addScriptRemote "http://momentjs.com/downloads/moment.min.js"
        addScriptRemote "http://vdw.github.io/HideSeek/javascripts/vendor/jquery.hideseek.min.js"
        $(widgetFile "view-draft")

utcTo8601 :: UTCTime -> String
utcTo8601 = formatTime defaultTimeLocale $ iso8601DateFormat (Just "%H:%M:%S%z")

isLeftToRightRow :: Draft -> Int -> Bool
isLeftToRightRow _ r = even r

pickNumToRC :: Draft -> Int -> (Int, Int)
pickNumToRC draft i = (r, c)
 where
    n = length $ draftParticipants draft
    r = i `div` n
    dir | isLeftToRightRow draft r = id
        | otherwise                = (pred n -)
    c = dir (i `mod` n)

rcToPickNum :: Draft -> (Int, Int) -> Int
rcToPickNum draft (r, c) = r * n + dir c
 where
    n = length $ draftParticipants draft
    dir | isLeftToRightRow draft r = id
        | otherwise                = flip subtract (pred n)

prettyTimeDiff :: NominalDiffTime -> String
prettyTimeDiff t = pref ++ concat
    [elide days "d ", elide hours "h ", show minutes ++ "m"]
 where
    elide n suff | n == 0    = ""
                 | otherwise = show n ++ suff
    posnegseconds = floor t :: Integer
    posneg = signum posnegseconds
    (days, dayremainder) = divMod (abs posnegseconds) (24*60*60)
    (hours, hourremainder) = divMod dayremainder (60*60)
    (minutes, _ ) = divMod hourremainder 60
    pref | posneg < 0 = "-"
         | otherwise = ""
