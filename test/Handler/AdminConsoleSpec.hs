module Handler.AdminConsoleSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "getAdminConsoleR" $ checkRequiresAdmin AdminConsoleR

        -- TODO figure out how to authenticate as admin/non-admin and check
        -- those cases too
