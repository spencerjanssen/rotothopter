module Common where

import Import

getCubeCards :: CubeId -> Handler [Text]
getCubeCards cuid = do
    Just (Cube _ cs) <- runDB $ get cuid
    return cs

getDraftPicks :: DraftId -> Handler [DraftPick]
getDraftPicks draftId = runDB $ do
    map entityVal <$> selectList [DraftPickDraftId ==. draftId] [Asc DraftPickPickNumber]

getDraft :: DraftId -> Handler Draft
getDraft did = do
    Just draft <- runDB $ get did
    return draft

pickOrder :: [a] -> [a]
pickOrder drafters = concat . repeat $ drafters ++ reverse drafters

-- returns Nothing if the draft is invalid
getNextDrafter :: Draft -> [DraftPick] -> Maybe UserId
getNextDrafter (Draft _ _ uids) picks = go (map draftPickDrafter picks) (pickOrder uids)
 where
    go [] (u:_) = Just u
    go (p:ps) (u:us) | p == u = go ps us
    go _ _ = Nothing