module Handler.ViewDraftInviteSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "getViewDraftInviteR" $ do
        it "displays the draft invite" $ do
            postCube testLargeCubeName testLargeCube
            Just (Entity cid _) <- runDB $ selectFirst ([] :: [Filter Cube]) []
            postDraftInvite cid
            Just (Entity _ inv) <- runDB $ selectFirst ([] :: [Filter DraftInvite]) []
            get $ ViewDraftInviteR $ inv ^. draftInviteHash
            statusIs 200