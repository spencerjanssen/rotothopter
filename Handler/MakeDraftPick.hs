module Handler.MakeDraftPick where

import Import
import Import.Mail
import Common
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Shakespeare.Text
import qualified Prelude (last)
import Handler.PrettyCard

getMakeDraftPickR :: DraftId -> Text -> Handler Html
getMakeDraftPickR draftId cardToPick = do
    uid <- requireAuthId
    draft <- getDraft draftId
    when (uid `notElem` draftParticipants draft) $ fail "you are not in this draft"
    picks <- getDraftPicks draftId
    mcardinfo <- maybeCardInfo cardToPick
    case getNextDrafter draft picks of
        Nothing -> fail "this draft has broken, contact administrator"
        Just uid'
            | uid /= uid' -> fail "it isn't your turn to pick yet"
            | otherwise ->
                defaultLayout $ do
                    setTitle "Make a draft pick"
                    $(widgetFile "post-makedraftpick")

postMakeDraftPickR :: DraftId -> Text -> Handler Html
postMakeDraftPickR draftId cardToPick = do
    uid <- requireAuthId
    draft <- getDraft draftId
    when (uid `notElem` draftParticipants draft) $ fail "you are not in this draft"
    picks <- getDraftPicks draftId
    case getNextDrafter draft picks of
        Nothing -> fail "this draft has broken, contact administrator"
        Just uid' | uid /= uid' -> fail "it isn't your turn to pick yet"
                  | otherwise -> actualPostMakeDraftPickR draftId uid (map draftPickCard picks) draft cardToPick

actualPostMakeDraftPickR draftId uid picks draft cardToPick = do
    allowedCards <- getPickAllowedCards draftId draft
    if cardToPick `elem` allowedCards
        then do
            t <- liftIO getCurrentTime
            dpid <- runDB $ insert $ DraftPick draftId (length picks) cardToPick uid t
            checkSendEmail draftId draft uid
            redirect (ViewDraftR draftId)
        else fail "you can't pick that card"

routeToTextUrl :: Route App -> Handler Text
routeToTextUrl route = withUrlRenderer $ \f -> f route []

checkSendEmail :: DraftId -> Draft -> UserId -> Handler ()
checkSendEmail draftId draft olduid = do
    picks <- getDraftPicks draftId

    case getNextDrafter draft picks of
        Just newuid | newuid /= olduid -> do
            Just user <- runDB $ get newuid
            let lastpick = Prelude.last picks
                round = 1 + ((length picks + 1) `div` length (draftParticipants draft))
            url <- routeToTextUrl (ViewDraftR draftId)
            Just lastpicker <- runDB $ get (draftPickDrafter lastpick)
            sendEmail user ("Time for draft round " ++ pack (show round)) $ [st|
#{pseudonym lastpicker} just drafted #{draftPickCard lastpick}.

It is time to make your pick.

#{url}
|]
        _ -> return ()
