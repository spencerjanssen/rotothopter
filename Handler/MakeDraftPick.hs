module Handler.MakeDraftPick where

import Import
import Import.Mail
import Common
import Text.Shakespeare.Text
import qualified Prelude (last)
import Handler.PrettyCard

getMakeDraftPickR :: DraftId -> Text -> Handler Html
getMakeDraftPickR draftId cardToPick = do
    uid <- requireAuthId
    draft <- getDraft draftId
    when (uid `onotElem` (draft ^. draftParticipants)) $ fail "you are not in this draft"
    picks <- getDraftPicks draftId
    mcardinfo <- maybeCardInfo cardToPick
    case getNextDrafter draft picks of
        Nothing -> fail "this draft has completed, you can't make a pick"
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
    when (uid `onotElem` view draftParticipants draft) $ fail "you are not in this draft"
    picks <- getDraftPicks draftId
    case getNextDrafter draft picks of
        Nothing -> fail "this draft has completed, you can't make a pick"
        Just uid' | uid /= uid' -> fail "it isn't your turn to pick yet"
                  | otherwise -> actualPostMakeDraftPickR draftId uid picks draft cardToPick

actualPostMakeDraftPickR :: DraftId -> Key User -> [DraftPick] -> Draft -> Text -> Handler b
actualPostMakeDraftPickR draftId uid picks draft cardToPick = do
    allowedCards <- getPickAllowedCards draftId draft
    if cardToPick `oelem` allowedCards
        then do
            t <- liftIO getCurrentTime
            let thepick = DraftPick draftId (length picks) cardToPick uid t
            _ <- runDB $ insert $ thepick
            checkSendEmail draftId draft uid
            notifyDraftWatcher thepick
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
                rnd = 1 + ((length picks + 1) `div` length (draft ^. draftParticipants))
            url <- routeToTextUrl (ViewDraftR draftId)
            Just lastpicker <- runDB $ get (lastpick ^. draftPickDrafter)
            sendEmail user ("Time for draft round " ++ pack (show rnd)) $ [st|
#{pseudonym lastpicker} just drafted #{lastpick ^. draftPickCard}.

It is time to make your pick.

#{url}
|]
        _ -> return ()
