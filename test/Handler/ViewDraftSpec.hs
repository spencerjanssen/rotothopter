module Handler.ViewDraftSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $
    describe "getViewDraftR" $
        it "displays a draft" $ do
            postCube testLargeCubeName testLargeCube
            postDraft testLargeCubeName testParticipants testDraftRounds
            did <- getOnlyDraftId
            get $ ViewDraftR did
            statusIs 200
            htmlCount "th" (length testParticipants + 2)
    -- TODO better tests that verify display of the draft table, available
    -- cards and which participant is ready to draft
