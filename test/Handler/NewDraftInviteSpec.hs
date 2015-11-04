module Handler.NewDraftInviteSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "getNewDraftInviteR" $ do
        checkRequiresAuth $ NewDraftInviteR (CubeKey 1)


    describe "postNewDraftInviteR" $ do
        it "posts a new draft invite" $ do
            postCube testLargeCubeName testLargeCube
            Just (Entity cid _) <- runDB $ selectFirst ([] :: [Filter Cube]) []
            postDraftInvite cid
            statusIs 303
