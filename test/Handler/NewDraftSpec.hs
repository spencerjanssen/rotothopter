module Handler.NewDraftSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "getNewDraftR" $ do
        checkRequiresAuth NewDraftR

    describe "postNewDraftR" $ do
        it "posts a new draft" $ do
            postCube testLargeCubeName testLargeCube
            --
            -- have all the users sign in so they're initialized in the DB:
            mapM_ authenticateAs testParticipants

            authenticateA
            get NewDraftR
            request $ do
                addToken
                byLabel "Cube Name" testLargeCubeName
                byLabel "Participants" (unlines testParticipants)
                byLabel "Rounds" (pack $ show testDraftRounds)
                setMethod "POST"
                setUrl NewDraftR
            statusIs 303
