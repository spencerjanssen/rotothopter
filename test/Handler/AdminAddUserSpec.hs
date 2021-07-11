module Handler.AdminAddUserSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "getAdminAddUserR" $
        checkRequiresAdmin AdminAddUserR

    describe "postAdminAddUserR" $
        it "allows admin to add a user" $ do
            authenticateAdmin
            get AdminAddUserR
            statusIs 200
            request $ do
                addToken
                byLabel "Email" x
                byLabel "Name" xname
                setMethod "POST"
                setUrl AdminAddUserR
            statusIs 303
            Just (Entity _ usr) <- runDB $ getBy $ UniqueUser x
            assertEqual "userIdent" x (usr ^. userIdent)
            assertEqual "userDisplayName" (Just xname) (usr ^. userDisplayName)
  where
    (x, xname) = ("x@test.com", "Xavier")
