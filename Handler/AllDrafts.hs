module Handler.AllDrafts where

import Import

getAllDraftsR :: Handler Html
getAllDraftsR = do
    drafts <- runDB $ do
        ds <- selectList ([] :: [Filter Draft]) []
        mapM (\(Entity did d) -> do Just cu <- get (draftCubeId d); return (did, d, cu)) ds
    defaultLayout $ do
        setTitle "All Drafts"
        $(widgetFile "all-drafts")
