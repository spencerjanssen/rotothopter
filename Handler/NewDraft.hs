module Handler.NewDraft where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Data.Text (strip)

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
    ((FormSuccess newDraft, _), _) <- runFormPost $ draftForm uid
    did <- runDB $ insert $ newDraft t
    redirect (ViewDraftR did)

draftForm :: Key User -> Form (UTCTime -> Draft)
draftForm uid = renderBootstrap3 BootstrapBasicForm $ Draft uid
    <$> (entityKey <$> areq cubeField "Cube Name" Nothing)
    <*> (map entityKey <$> areq participantsField "Participants" Nothing)
    <*> areq intField "Rounds" (Just 45)
 where
    participantsField = checkMMap findParticipants (Textarea . unlines . map (userIdent . entityVal)) textareaField
    findParticipants txt = do
        let names = map strip . lines $ unTextarea txt :: [Text]
        validatedNames <- runDB $
            forM names $
                \n -> maybe (Left n) Right <$> getBy (UniqueUser n)
        return $ sequence validatedNames
    cubeField = checkMMap findCube (cubeCubeName . entityVal) textField
    findCube txt =
        maybe (Left txt) Right <$> runDB (getBy $ UniqueCubeName txt)
