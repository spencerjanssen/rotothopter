module Handler.ViewDraft where

import Import
import Common
import Handler.PrettyCard
import Data.Time
import qualified Data.Map as Map
import qualified Database.Esqueleto as E

getViewDraftR :: DraftId -> Handler Html
getViewDraftR draftId = do
    Just draft <- runDB $ get draftId
    Just participants <- sequence <$> runDB (mapM get $ draft ^. draftParticipants)
    Just (Cube _ cubename) <- runDB $ get $ draft ^. draftCubeId
    picks <- getPicksAndInfo draftId
    muid <- maybeAuthId
    allowedCards <- getAllowedCards draftId (draft ^. draftCubeId)
    let pickmap = Map.fromList $ map (\p -> (p ^. _1.pickNumber, p)) picks
        picksOnly = map fst picks
        lastRow = min (fromIntegral $ view draftRounds draft-1) . fst
                . pickNumToRC draft $ Map.size pickmap
        mnextdrafter = getNextDrafter draft picksOnly
        timediffByCell = Map.fromList $ do
            (p1, p2) <- zip picksOnly (drop 1 picksOnly)
            return ( p2 ^. pickNumber
                   , diffUTCTime (p2 ^. pickCreated) (p1 ^. pickCreated))
        timediffByCol c = sum $ do
            r <- [0 .. lastRow]
            Just d <- return $ Map.lookup (rcToPickNum draft (r, c)) timediffByCell
            return d
        timediffByRow r = sum $ do
            c <- zipWith const [0 ..] participants
            Just d <- return $ Map.lookup (rcToPickNum draft (r, c)) timediffByCell
            return d
        (isNextDrafter, draftdone)
         = case (mnextdrafter, muid) of
                (Just nextdrafter, Just uid)
                    | uid == nextdrafter -> (True, False)
                (Nothing, _) -> (False, True)
                _ -> (False, False)
    let catcards = categorizeCardList allowedCards
    timestamp <- (elem "timestamp" . map fst . reqGetParams) <$> getRequest
    defaultLayout $ do
        setTitle $ if isNextDrafter
                    then "***Your turn to pick!"
                    else "View Cube Draft"
        addScript (StaticR js_jquery_timeago_js)
        addScript (StaticR js_moment_min_js)
        addScript (StaticR js_jquery_hideseek_min_js)
        addScript (StaticR js_jquery_stickytableheaders_min_js)
        $(widgetFile "view-draft")

getPicksAndInfo :: DraftId -> Handler [(Pick, Maybe Card)]
getPicksAndInfo did = map munge <$> runDB query
 where
    munge (x, my) = (entityVal x, entityVal <$> my)
    query = E.select $ E.from $ \(pick `E.LeftOuterJoin` card) -> do
                E.on $ E.just (pick E.^. PickCard) E.==. card E.?. CardName
                E.where_ $ pick E.^. PickDraft E.==. E.val did
                return (pick, card)

getAllowedCards :: DraftId -> CubeId -> Handler [Either Text Card]
getAllowedCards did cuid = map munge <$> runDB query
 where
    munge (E.Value x, my) = maybe (Left x) Right (entityVal <$> my)
    subquery = E.from $ \pick -> do
                E.where_ $ pick E.^. PickDraft E.==. E.val did
                return (pick E.^. PickCard)
    query = E.select $ E.from $ \(cubeCard `E.LeftOuterJoin` card) -> do
                E.on $ E.just (cubeCard E.^. CubeCardName) E.==. card E.?. CardName
                E.where_ $ cubeCard E.^. CubeCardCube E.==. E.val cuid
                E.where_ $ cubeCard E.^. CubeCardName `E.notIn` E.subList_select subquery
                return (cubeCard E.^. CubeCardName, card)

utcTo8601 :: UTCTime -> String
utcTo8601 = formatTime defaultTimeLocale $ iso8601DateFormat (Just "%H:%M:%S%z")

isLeftToRightRow :: Draft -> Int -> Bool
isLeftToRightRow = const even

pickNumToRC :: Draft -> Int -> (Int, Int)
pickNumToRC draft i = (r, c)
 where
    n = length $ draft ^. draftParticipants
    r = i `div` n
    dir | isLeftToRightRow draft r = id
        | otherwise                = (pred n -)
    c = dir (i `mod` n)

rcToPickNum :: Draft -> (Int, Int) -> Int
rcToPickNum draft (r, c) = r * n + dir c
 where
    n = length $ draft ^. draftParticipants
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
