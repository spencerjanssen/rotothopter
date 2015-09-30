module Handler.NewDraft where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Data.Text (strip)

getNewDraftR :: Handler Html
getNewDraftR = do
    (formWidget, formEnctype) <- generateFormPost draftForm
    defaultLayout $ do
        setTitle "Start a new draft"
        $(widgetFile "post-newdraft")

postNewDraftR :: Handler Html
postNewDraftR = error "Not yet implemented: postNewDraftR"

draftForm :: Form Draft
draftForm = renderBootstrap3 BootstrapBasicForm $ Draft
    <$> (entityKey <$> areq cubeField "Cube Name" Nothing)
    <*> (map entityKey <$> areq participantsField "Participants" Nothing)
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
        maybe (Left txt) Right <$> (runDB $ getBy $ UniqueCubeName txt)
