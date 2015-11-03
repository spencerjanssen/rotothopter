module Handler.NewDraftInviteSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "getNewDraftInviteR" $ do
        checkRequiresAuth NewDraftInviteR


    describe "postNewDraftInviteR" $ do
        it "posts a new draft invite" $ do
            postCube testLargeCubeName testLargeCube
            authenticateA
            get NewDraftInviteR
            request $ do
                addToken
                byLabel "Cube Name" testLargeCubeName
                byLabel "Rounds" "45"
                setMethod "POST"
                setUrl NewDraftInviteR
