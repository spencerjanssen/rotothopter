module Handler.ViewDraft where

import Import
import Common
import Handler.PrettyCard
import Data.Time.LocalTime
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
        (lastRow, _) = pickNumToRC draft $ Map.size pickmap
        mnextdrafter = getNextDrafter draft picks
        isNextDrafter = case (mnextdrafter, muid) of
                            (Just nextdrafter, Just uid)
                                | uid == nextdrafter -> True
                            _ -> False
    catcards <- categorizeUnknownCardList allowedCards
    tz <- liftIO getCurrentTimeZone
    timestamp <- (elem "timestamp" . map fst . reqGetParams) <$> getRequest
    defaultLayout $ do
        setTitle "View Cube Draft"
        $(widgetFile "view-draft")

timestampForm :: Form Bool
timestampForm = renderTable (maybe False id <$> aopt boolField "timestamp" Nothing)

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
