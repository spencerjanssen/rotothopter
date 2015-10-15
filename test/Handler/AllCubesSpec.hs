module Handler.AllCubesSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $
    describe "getAllCubesR" $
        it "lists the posted cube" $ do
            postTestCube
            get AllCubesR
            statusIs 200
            htmlCount "#main li" 1
