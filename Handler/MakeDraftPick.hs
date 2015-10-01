module Handler.MakeDraftPick where

import Import
import Import.Mail
import Common
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Shakespeare.Text
import qualified Prelude (last)

getMakeDraftPickR :: DraftId -> Handler Html
getMakeDraftPickR draftId = do
    uid <- requireAuthId
    draft <- getDraft draftId
    when (uid `notElem` draftParticipants draft) $ fail "you are not in this draft"
    picks <- getDraftPicks draftId
    case getNextDrafter draft picks of
        Nothing -> fail "this draft has broken, contact administrator"
        Just uid' | uid /= uid' -> fail "it isn't your turn to pick yet"
                  | otherwise -> actualGetMakeDraftPickR draftId uid (map draftPickCard picks) draft

actualGetMakeDraftPickR draftId uid picks draft = do
    allowedCards <- getPickAllowedCards draftId draft
    (formWidget, formEnctype) <- generateFormPost $ draftPickForm draftId (length picks) uid allowedCards
    defaultLayout $ do
        setTitle "Make a draft pick"
        $(widgetFile "post-makedraftpick")

postMakeDraftPickR :: DraftId -> Handler Html
postMakeDraftPickR draftId = do
    uid <- requireAuthId
    draft <- getDraft draftId
    when (uid `notElem` draftParticipants draft) $ fail "you are not in this draft"
    picks <- getDraftPicks draftId
    case getNextDrafter draft picks of
        Nothing -> fail "this draft has broken, contact administrator"
        Just uid' | uid /= uid' -> fail "it isn't your turn to pick yet"
                  | otherwise -> actualPostMakeDraftPickR draftId uid (map draftPickCard picks) draft

actualPostMakeDraftPickR draftId uid picks draft = do
    allowedCards <- getPickAllowedCards draftId draft
    ((FormSuccess newDraftPick, __), _) <- runFormPost $ draftPickForm draftId (length picks) uid allowedCards
    dpid <- runDB $ insert newDraftPick
    checkSendEmail draftId draft uid
    redirect (ViewDraftR draftId)

routeToTextUrl :: Route App -> Handler Text
routeToTextUrl route = withUrlRenderer $ \f -> f route []

checkSendEmail :: DraftId -> Draft -> UserId -> Handler ()
checkSendEmail draftId draft olduid = do
    picks <- getDraftPicks draftId

    case getNextDrafter draft picks of
        Just newuid | newuid /= olduid -> do
            Just user <- runDB $ get newuid
            let lastpick = Prelude.last picks
                round = (length picks + 1) `div` length (draftParticipants draft)
            url <- routeToTextUrl (ViewDraftR draftId)
            Just lastpicker <- runDB $ get (draftPickDrafter lastpick)
            sendEmail user ("Time for draft round " ++ pack (show round)) $ [st|
#{pseudonym lastpicker} just drafted #{draftPickCard lastpick}.

It is time to make your pick.

#{url}
|]
        _ -> return ()

draftPickForm :: DraftId -> Int -> UserId -> [Text] -> Form DraftPick
draftPickForm did picknum uid allowedCards =
    renderBootstrap3 BootstrapBasicForm $ DraftPick did picknum
    <$> areq (selectFieldList labeledCards) "Card to Pick" Nothing
    <*> pure uid
 where
    labeledCards = zip allowedCards allowedCards
