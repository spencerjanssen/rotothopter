module Handler.MakeDraftPickSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "getMakeDraftPickR" $
        it "grants access to the correct person" $ do
            did <- initialize
            let (a:b:_) = testParticipants
                (c:_) = testLargeCube
            authenticateAs b
            get $ MakeDraftPickR did c
            statusIs 500
            authenticateAs a
            get $ MakeDraftPickR did c
            statusIs 200
            -- TODO we don't handle this yet
            -- get $ MakeDraftPickR did "card that isn't in the cube"
            -- statusIs 500

    describe "postMakeDraftPickR" $
        it "completes a draft successfully" $ do
            did <- initialize
            forM_ picks $ \(i, good_p) -> do
                -- verify that the other participants can't pick right now:
                forM_ (filter (/= good_p) testParticipants) $ \test_p -> do
                    tryPick did test_p (cardOf i)
                    statusIs 500
                -- verify that good_p can't take a card that has been taken:
                tryPick did good_p (cardOf (pred i))
                statusIs 500
                -- finally make a valid pick:
                tryPick did good_p (cardOf i)
                statusIs 303
            -- once the draft is over, verify nobody can make a pick
            forM_ testParticipants $ \p -> do
                tryPick did p (cardOf (length testParticipants * testDraftRounds + 1))
                statusIs 500
 where
    initialize = do
        postCube testLargeCubeName testLargeCube
        postDraft testLargeCubeName testParticipants testDraftRounds
        getOnlyDraftId
    snake ps = let x = ps ++ reverse ps ++ x in x
    picks = take (length testParticipants *  testDraftRounds)
          $ zip [1 :: Int ..] $ snake testParticipants
    tryPick did p c = do
        authenticateAs p
        post $ MakeDraftPickR did c
    cardOf n = "Card_" ++ pack (show n)
