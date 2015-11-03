module Handler.JoinDraftInviteSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "postJoinDraftInviteR" $ do
        it "lets the user join a draft" $ do
            postCube testLargeCubeName testLargeCube
            postDraftInvite testLargeCubeName
            Just (Entity _ inv) <- runDB $ selectFirst ([] :: [Filter DraftInvite]) []
            post $ JoinDraftInviteR $ inv ^. draftInviteHash
            statusIs 303
