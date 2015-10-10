module Handler.AdminConsoleSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "getAdminConsoleR" $ checkRequiresAdmin AdminConsoleR
