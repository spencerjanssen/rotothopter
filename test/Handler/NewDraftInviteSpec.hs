module Handler.NewDraftInviteSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "getNewDraftInviteR" $ do
        checkRequiresAuth NewDraftInviteR


    describe "postNewDraftInviteR" $ do
        it "posts a new draft invite" $ do
            postCube testLargeCubeName testLargeCube
            postDraftInvite testLargeCubeName
            statusIs 303
