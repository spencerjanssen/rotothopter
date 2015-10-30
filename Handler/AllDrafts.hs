module Handler.AllDrafts where

import Import

getAllDraftsR :: Handler Html
getAllDraftsR = do
    drafts <- runDB $ do
        ds <- selectList ([] :: [Filter Draft]) []
        forM ds $ \(Entity did d) -> do
            Just cu <- get (view draftCube d)
            return (did, d, cu)
    defaultLayout $ do
        setTitle "All Drafts"
        $(widgetFile "all-drafts")
