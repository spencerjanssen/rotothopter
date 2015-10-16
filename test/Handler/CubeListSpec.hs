module Handler.CubeListSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "getNewCubeListR" $
        it "loads the page" $ do
            get NewCubeListR
            statusIs 200

    describe "postNewCubeListR" $
        it "inserts the new cube in the DB" $ do
            postTestCube
            statusIs 303
            Just (Entity _ cl) <- runDB $ getBy (UniqueCubeName testCubeName)
            assertEqual "cube list" (cl ^. cubeCubeList) testCubeList

    describe "getViewCubeListR" $
        it "displays the cube" $ do
            postTestCube
            Just (Entity id _) <- runDB $ getBy $ UniqueCubeName testCubeName
            get $ ViewCubeListR id
            statusIs 200
