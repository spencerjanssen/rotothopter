module Handler.UserProfileSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "getUserProfileR" $
        checkRequiresAuth UserProfileR

    describe "postUserProfileR" $
        it "allows user to update name" $ do
            authenticateA
            get UserProfileR
            statusIs 200
            request $ do
                addToken
                byLabel "Your name" "Adam"
                setMethod "POST"
                setUrl UserProfileR
            statusIs 303
            Just (Entity _ usr) <- runDB $ getBy $ UniqueUser "a@test.com"
            assertEqual "userDisplayName" (Just "Adam") (usr ^. userDisplayName)
