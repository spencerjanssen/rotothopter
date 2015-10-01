module Handler.ViewDraft where

import Import
import Common
import Handler.PrettyCard

getViewDraftR :: DraftId -> Handler Html
getViewDraftR draftId = do
    Just draft <- runDB $ get draftId
    Just participants <- sequence <$> (runDB $ mapM get $ draftParticipants draft)
    Just (Cube cubename _) <- runDB $ get $ draftCubeId draft
    picks <- getDraftPicks draftId
    muid <- maybeAuthId
    let snaked = snakeTable (length participants) picks
        mnextdrafter = getNextDrafter draft picks
    defaultLayout $ do
        setTitle "View Cube Draft"
        $(widgetFile "view-draft")

padTo :: Int -> a -> [a] -> [a]
padTo n p [] = replicate n p
padTo n p (x:xs) = x : padTo (n-1) p xs

data Direction = GoingRight | GoingLeft

chopBy :: Int -> [a] -> [[a]]
chopBy _ [] = []
chopBy n xs = case splitAt n xs of (ys, zs) -> ys : chopBy n zs

mapLast :: (a -> a) -> [a] -> [a]
mapLast f [x] = [f x]
mapLast f (x:xs) = x : mapLast f xs
mapLast _ [] = []

snakeTable :: Int -> [a] -> [(Int, Direction, [Maybe a])]
snakeTable n xs = zipWith3 dirify [1 ..] ds . mapLast (padTo n Nothing) . chopBy n . map Just $ xs
 where
    ds = GoingRight : GoingLeft : ds
    dirify n GoingRight x = (n, GoingRight, x)
    dirify n GoingLeft x = (n, GoingLeft, reverse x)
