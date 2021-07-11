module Handler.PicksByParticipantSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "getPicksByParticipantR" $ do
        it "loads the page" $ do
            postCube testLargeCubeName testLargeCube
            postDraft testLargeCubeName testParticipants testDraftRounds
            did <- getOnlyDraftId
            uid <- getFirstDrafter did
            -- ugly...
            get $ PicksByParticipantR did $ UserKey 1
            statusIs 200

getFirstDrafter did = do
    Just (Entity _ x) <- runDB $ selectFirst [DraftParticipantDraft ==. did] []
    return $ x ^. draftParticipantDrafter
