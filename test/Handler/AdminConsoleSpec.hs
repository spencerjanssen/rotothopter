module Handler.AdminConsoleSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $
    describe "getAdminConsoleR" $ checkRequiresAdmin AdminConsoleR
