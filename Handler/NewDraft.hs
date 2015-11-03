module Handler.NewDraft where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Data.Text (strip)
import Common (bootstrapLabel)

getNewDraftR :: Handler Html
getNewDraftR = do
    uid <- requireAuthId
    (formWidget, formEnctype) <- generateFormPost $ draftForm uid
    defaultLayout $ do
        setTitle "Start a new draft"
        $(widgetFile "post-newdraft")

postNewDraftR :: Handler Html
postNewDraftR = do
    uid <- requireAuthId
    t <- liftIO getCurrentTime
    ((FormSuccess (newDraft, uids), _), _) <- runFormPost $ draftForm uid
    did <- runDB $ do
        did <- insert $ newDraft t
        forM_ (zip uids [0 ..]) $ \(u, i) -> void $ insert (DraftParticipant u did i)
        return did
    redirect (ViewDraftR did)

draftForm :: Key User -> Form (UTCTime -> Draft, [UserId])
draftForm uid = renderBootstrap3 BootstrapBasicForm $ mk
    <$> (entityKey <$> areq cubeField (bootstrapLabel "Cube Name") Nothing)
    <*> (map entityKey <$> areq participantsField (bootstrapLabel "Participants") Nothing)
    <*> areq intField "Rounds" (Just 45)
 where
    mk cn ps rs = (Draft uid cn (fromIntegral $ length ps) rs, ps)
    participantsField = checkMMap findParticipants (Textarea . unlines . map (view userIdent . entityVal)) textareaField
    findParticipants txt = do
        let names = map strip . lines $ unTextarea txt :: [Text]
        validatedNames <- runDB $
            forM names $
                \n -> maybe (Left n) Right <$> getBy (UniqueUser n)
        return $ sequence validatedNames
    cubeField = checkMMap findCube (view cubeName . entityVal) textField
    findCube txt =
        maybe (Left txt) Right <$> runDB (getBy $ UniqueCubeName txt)
