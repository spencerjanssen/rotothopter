module Handler.ViewDraft where

import Import
import Common
import Handler.PrettyCard
import Data.Time
import qualified Data.Map as Map
import qualified Database.Esqueleto as E

getViewDraftR :: DraftId -> Handler Html
getViewDraftR draftId = do
    draft <- runDB $ get404 draftId
    participants <- getParticipants draftId
    commissioner <- isCommissioner draftId
    mnextdrafter <- getNextDrafter (Entity draftId draft)
    Just (Cube _ cubename) <- runDB $ get $ draft ^. draftCube
    picks <- getPicksAndInfo draftId Nothing
    muid <- maybeAuthId
    allowedCards <- getAllowedCards draftId (draft ^. draftCube)
    let pickmap = Map.fromList $ map (\p -> (p ^. _1.pickNumber, p)) picks
        picksOnly = map fst picks
        lastRow = min (fromIntegral $ view draftRounds draft-1) . fst
                . pickNumToRC draft $ Map.size pickmap
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
        linkpick = do
            uid <- muid
            guard (uid `elem` map entityKey participants)
            Just $ \cname ->
                    [whamlet|
                    <a href=@{MakeDraftPickR draftId cname}>#{cname}
                    |]
        isParticipant = maybe False (`elem` map entityKey participants) muid
    timestamp <- (elem "timestamp" . map fst . reqGetParams) <$> getRequest
    let prefix = if isNextDrafter then "***" else ""
        title = case mnextdrafter >>= \nextId -> find (\d -> entityKey d == nextId) participants of
            Nothing -> toHtml cubename ++ " draft completed!"
            Just next -> prefix ++ "Pick " ++ toHtml (succ lastRow) ++ ", " ++ toHtml (pseudonym $ entityVal next) ++ " drafting " ++ toHtml cubename
    defaultLayout $ do
        setTitle title
        addScript (StaticR js_jquery_timeago_js)
        addScript (StaticR js_moment_min_js)
        addScript (StaticR js_jquery_hideseek_min_js)
        addScript (StaticR js_jquery_stickytableheaders_min_js)
        $(widgetFile "view-draft")

getAllowedCards :: DraftId -> CubeId -> Handler [Card]
getAllowedCards did cuid = map munge <$> runDB query
 where
    munge (Entity _ x) = x
    subquery = E.from $ \pick -> do
                E.where_ $ pick E.^. PickDraft E.==. E.val did
                return (pick E.^. PickCard)
    query = E.select $ E.from $ \(cubeEntry `E.InnerJoin` card) -> do
                E.on $ cubeEntry E.^. CubeEntryCard E.==. card E.^. CardId
                E.where_ $ cubeEntry E.^. CubeEntryCube E.==. E.val cuid
                E.where_ $ cubeEntry E.^. CubeEntryCard `E.notIn` E.subList_select subquery
                return card

utcTo8601 :: UTCTime -> String
utcTo8601 = formatTime defaultTimeLocale $ iso8601DateFormat (Just "%H:%M:%S%z")

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

getReservedCardsR :: DraftId -> Handler Value
getReservedCardsR draftId = do
    userId <- requireAuthId
    res <- runDB $
        map (unCardKey . _pickReservationCard . entityVal) <$> selectList [PickReservationDrafter ==. userId, PickReservationDraft ==. draftId] [Asc PickReservationNumber]
    returnJson res
