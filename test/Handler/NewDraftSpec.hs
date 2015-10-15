module Handler.NewDraftSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "getNewDraftR" $
        checkRequiresAuth NewDraftR

    describe "postNewDraftR" $
        it "posts a new draft" $ do
            postCube testLargeCubeName testLargeCube
            authenticateA
            postDraft testLargeCubeName testParticipants testDraftRounds
            statusIs 303
