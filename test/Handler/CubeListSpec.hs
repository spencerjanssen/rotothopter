module Handler.CubeListSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "getNewCubeListR" $ do
        it "loads the page" $ do
            get NewCubeListR
            statusIs 200

    describe "postNewCubeListR" $ do
        it "inserts the new cube in the DB" $ do
            postTestCube
            statusIs 303
            Just (Entity _ cl) <- runDB $ getBy (UniqueCubeName testCubeName)
            assertEqual "cube list" (cubeCubeList cl) testCubeList

    describe "getViewCubeListR" $ do
        it "displays the cube" $ do
            postTestCube
            Just (Entity id _) <- runDB $ getBy $ UniqueCubeName testCubeName
            get $ ViewCubeListR id
            statusIs 200
