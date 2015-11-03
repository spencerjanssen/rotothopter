module Common where

import Import
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Database.Esqueleto as E
import Yesod.Form.Bootstrap3 (bfs)

bootstrapLabel :: Text -> FieldSettings site
bootstrapLabel x = bfs (x :: Text)

textAreaHeight :: Int -> FieldSettings site -> FieldSettings site
textAreaHeight n s = s {fsAttrs = ("style", css) : fsAttrs s}
 where
    css = "height: " ++ pack (show n) ++ "em"

getCubeCards :: CubeId -> Handler [Text]
getCubeCards cuid = do
    cs <- runDB $ selectList [CubeEntryCube ==. cuid] []
    return $ map (unCardKey . view cubeEntryCard . entityVal) cs

getPicks :: DraftId -> Handler [Pick]
getPicks draftId = runDB $
    map entityVal <$> selectList [PickDraft ==. draftId] [Asc PickNumber]

getDraft :: DraftId -> Handler Draft
getDraft did = do
    Just draft <- runDB $ get did
    return draft

pickOrder :: [a] -> [a]
pickOrder drafters = concat . repeat $ drafters ++ reverse drafters

getNextDrafter :: Entity Draft -> Handler (Maybe UserId)
getNextDrafter (Entity did d) =
    runDB $ do
        mpick <- selectFirst [PickDraft ==. did] [Desc PickNumber, LimitTo 1]
        let nextpick = fromIntegral $ maybe 0 (succ . view pickNumber) (entityVal <$> mpick)
            (rd, nextseat) = pickNumToRC d nextpick
        if fromIntegral rd >= d ^. draftRounds
            then return Nothing
            else do
                s <- getBy (UniqueDraftSeat did (fromIntegral nextseat))
                return (view draftParticipantDrafter . entityVal <$> s)

pickNumToRC :: Draft -> Int -> (Int, Int)
pickNumToRC draft i = (r, c)
 where
    n = fromIntegral $ draft ^. draftParticipants
    r = i `div` n
    dir | isLeftToRightRow draft r = id
        | otherwise                = (pred n -)
    c = dir (i `mod` n)

rcToPickNum :: Draft -> (Int, Int) -> Int
rcToPickNum draft (r, c) = r * n + dir c
 where
    n = fromIntegral $ draft ^. draftParticipants
    dir | isLeftToRightRow draft r = id
        | otherwise                = flip subtract (pred n)

isLeftToRightRow :: Draft -> Int -> Bool
isLeftToRightRow = const even

getParticipants :: DraftId -> Handler [Entity User]
getParticipants did = runDB query
 where
    query = E.select $ E.from $ \(dp `E.InnerJoin` user) -> do
                E.on $ dp E.^. DraftParticipantDrafter E.==. user E.^. UserId
                E.where_ $ dp E.^. DraftParticipantDraft E.==. E.val did
                E.orderBy [E.asc (dp E.^. DraftParticipantSeat)]
                return user

getPickAllowedCards :: DraftId -> Draft -> Handler [Text]
getPickAllowedCards did draft = do
    cubeCards <- getCubeCards (draft ^. draftCube)
    picks <- map (unCardKey . view pickCard) <$> getPicks did
    return (Set.toList (Set.fromList cubeCards Set.\\ Set.fromList picks))

withDraftWatch :: DraftId -> Maybe (STM a) -> (TChan Pick -> STM a) -> Handler (STM a)
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

notifyDraftWatcher :: Pick -> Handler ()
notifyDraftWatcher dp
 = atomically =<< withDraftWatch
                    (dp ^. pickDraft)
                    (Just $ return ())
                    (`writeTChan` dp)

subscribeDraftWatcher :: DraftId -> Handler (TChan Pick)
subscribeDraftWatcher did
 = atomically =<< withDraftWatch did Nothing dupTChan
