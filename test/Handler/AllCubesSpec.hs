module Handler.AllCubesSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "getAllCubesR" $ do
        it "lists the posted cube" $ do
            postTestCube
            get AllCubesR
            statusIs 200
            htmlCount "#main li" 1
