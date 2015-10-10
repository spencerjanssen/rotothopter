module Handler.NewDraftSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "getNewDraftR" $ do
        checkRequiresAuth NewDraftR

    describe "postNewDraftR" $ do
        it "posts a new draft" $ do
            postCube testLargeCubeName testLargeCube
            authenticateA
            postDraft testLargeCubeName testParticipants testDraftRounds
            statusIs 303
