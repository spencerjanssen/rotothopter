module Handler.ViewDraft where

import Import

getViewDraftR :: DraftId -> Handler Html
getViewDraftR draftId = do
    Just draft <- runDB $ get draftId
    Just participants <- (fmap (map userIdent) . sequence) <$> (runDB $ mapM get $ draftParticipants draft)
    Just (Cube cubename _) <- runDB $ get $ draftCubeId draft
    picks <- getPicks draftId
    defaultLayout $ do
        setTitle "View Cube Draft"
        $(widgetFile "view-draft")

getPicks :: DraftId -> Handler [DraftPick]
getPicks draftId = runDB $ do
    map entityVal <$> selectList [DraftPickDraftId ==. draftId] [Asc DraftPickPickNumber]
