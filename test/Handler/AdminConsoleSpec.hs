module Handler.AdminConsoleSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "getAdminConsoleR" $ do
        it "redirects for user that is not logged in" $ do
            get AdminConsoleR
            statusIs 303
        it "fails for non-admin" $ do
            authenticateA
            get AdminConsoleR
            statusIs 403
        it "succeeds for admin" $ do
            authenticateAdmin
            get AdminConsoleR
            statusIs 200

        -- TODO figure out how to authenticate as admin/non-admin and check
        -- those cases too
