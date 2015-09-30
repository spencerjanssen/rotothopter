module Handler.ViewDraft where

import Import
import Common

getViewDraftR :: DraftId -> Handler Html
getViewDraftR draftId = do
    Just draft <- runDB $ get draftId
    Just participants <- (fmap (map userIdent) . sequence) <$> (runDB $ mapM get $ draftParticipants draft)
    Just (Cube cubename _) <- runDB $ get $ draftCubeId draft
    let dpdata dp = do
        Just u <- runDB $ get (draftPickDrafter dp)
        return (userIdent u, dp)
    rawpicks <- getDraftPicks draftId
    picks <- mapM dpdata rawpicks
    muid <- maybeAuthId
    let snaked = snakeTable (length participants) False (map (draftPickCard . snd) picks) :: [[Maybe Text]]
        mnextdrafter = getNextDrafter draft rawpicks
    defaultLayout $ do
        setTitle "View Cube Draft"
        $(widgetFile "view-draft")

padTo :: Int -> a -> [a] -> [a]
padTo n p [] = replicate n p
padTo n p (x:xs) = x : padTo (n-1) p xs

-- | snakeTable cols revThisLine items
snakeTable :: Int -> Bool -> [a] -> [[Maybe a]]
snakeTable 0 _ _ = error "Can't snake with 0 columns"
snakeTable _ _ [] = []
snakeTable n rev ps
 = (trans . padTo n Nothing $ map Just row)
 : snakeTable n (not rev) rest
 where
    (row, rest) = splitAt n ps
    trans | rev       = reverse
          | otherwise = id
