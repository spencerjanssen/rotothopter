module Handler.AllDraftsSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "getAllDraftsR" $ do
        it "shows the single posted draft" $ do
            postCube testLargeCubeName testLargeCube
            postDraft testLargeCubeName testParticipants testDraftRounds
            get AllDraftsR
            statusIs 200
            htmlCount "#main li" 1
