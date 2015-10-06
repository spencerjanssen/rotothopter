module Common where

import Import
import qualified Data.Set as Set
import qualified Data.Map as Map

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

-- returns Nothing if the draft is complete
getNextDrafter :: Draft -> [DraftPick] -> Maybe UserId
getNextDrafter (Draft _ _ uids n _) picks = go 0 (map draftPickDrafter picks) (pickOrder uids)
 where
    maxPick = fromIntegral (length uids) * n
    go i _ _ | i >= maxPick = Nothing
    go _ [] (u:_) = Just u
    go i (p:ps) (u:us) | p == u = go (succ i) ps us
    go _ _ _ = error "this draft is invalid"

getPickAllowedCards :: DraftId -> Draft -> Handler [Text]
getPickAllowedCards did draft = do
    cubeCards <- getCubeCards (draftCubeId draft)
    picks <- map draftPickCard <$> getDraftPicks did
    return (Set.toList (Set.fromList cubeCards Set.\\ Set.fromList picks))

withDraftWatch :: DraftId -> Maybe (STM a) -> (TChan DraftPick -> STM a) -> Handler (STM a)
withDraftWatch did readFail f = do
    tmp <- appDraftWatchers <$> ask
    return $ do
        mp <- readTVar tmp
        case Map.lookup did mp of
            Just x -> f x
            Nothing -> case readFail of
                Nothing -> do
                    x <- newBroadcastTChan
                    writeTVar tmp (Map.insert did x mp)
                    f x
                Just act -> act

notifyDraftWatcher :: DraftPick -> Handler ()
notifyDraftWatcher dp
 = atomically =<< withDraftWatch
                    (draftPickDraftId dp)
                    (Just $ return ()) 
                    (\tc -> writeTChan tc dp)

subscribeDraftWatcher :: DraftId -> Handler (TChan DraftPick)
subscribeDraftWatcher did
 = atomically =<< withDraftWatch did Nothing dupTChan
