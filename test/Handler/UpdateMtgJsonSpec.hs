module Handler.UpdateMtgJsonSpec (spec) where

import TestImport

spec :: Spec
spec =
    withApp $
        describe "getUpdateMtgJsonR" $
            checkRequiresAdmin UpdateMtgJsonR

{-
 - TODO, figure out how to intercept the HTTP request this handler makes to
 - insert a fake/abridged mtgjson document.  Downloading the entire 10 MB
 - file is both expensive and nondeterministic.
describe "postUpdateMtgJsonR" $ do
    error "Spec not implemented: postUpdateMtgJsonR"
-}
