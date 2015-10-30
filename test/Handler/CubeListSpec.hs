module Handler.CubeListSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "getNewCubeListR" $
        it "loads the page" $ do
            authenticateA
            get NewCubeListR
            statusIs 200

    describe "postNewCubeListR" $
        it "inserts the new cube in the DB" $ do
            postTestCube
            statusIs 303
            cs <- runDB $ do
                Just (Entity ci c) <- getBy (UniqueCubeName testCubeName)
                map (unCardKey . view cubeEntryCard . entityVal) <$> selectList [CubeEntryCube ==. ci] []
            assertEqual "cube list" (sort cs) (sort testCubeList)

    describe "getViewCubeListR" $
        it "displays the cube" $ do
            postTestCube
            Just (Entity id _) <- runDB $ getBy $ UniqueCubeName testCubeName
            get $ ViewCubeListR id
            statusIs 200
