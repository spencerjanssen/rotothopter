module Handler.AdminConsoleSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "getAdminConsoleR" $ do
        it "redirects for user that is not logged in" $ do
            get AdminConsoleR
            statusIs 303

        -- TODO figure out how to authenticate as admin/non-admin and check
        -- those cases too
