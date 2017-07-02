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
    drafters <- map entityKey <$> getParticipants draftId
    when (uid `onotElem` drafters ) $ fail "you are not in this draft"
    mcardinfo <- maybeCardInfo cardToPick
    mnext <- getNextDrafter (Entity draftId draft)
    case mnext of
        Nothing -> fail "this draft has completed, you can't make a pick"
        Just nextDrafter ->
                defaultLayout $ do
                    setTitle "Make a draft pick"
                    $(widgetFile "post-makedraftpick")

postMakeDraftPickR :: DraftId -> Text -> Handler Html
postMakeDraftPickR draftId cardToPick = do
    uid <- requireAuthId
    draft <- getDraft draftId
    drafters <- map entityKey <$> getParticipants draftId
    when (uid `onotElem` drafters) $ fail "you are not in this draft"
    picks <- getPicks draftId
    mnext <- getNextDrafter (Entity draftId draft)
    case mnext of
        Nothing -> fail "this draft has completed, you can't make a pick"
        Just uid' | uid /= uid' -> fail "it isn't your turn to pick yet"
                  | otherwise -> actualPostMakeDraftPickR draftId uid picks draft cardToPick

actualPostMakeDraftPickR :: DraftId -> Key User -> [Pick] -> Draft -> Text -> Handler b
actualPostMakeDraftPickR draftId uid picks draft cardToPick = do
    allowedCards <- getPickAllowedCards draftId draft
    if cardToPick `oelem` allowedCards
        then do
            t <- liftIO getCurrentTime
            let thepick = Pick draftId (length picks) (draft ^. draftCube) (CardKey cardToPick) uid t
            _ <- runDB $ insert $ thepick
            checkSendEmail draftId draft uid
            notifyDraftWatcher thepick
            redirect (ViewDraftR draftId)
        else fail "you can't pick that card"

postReserveDraftPickR :: DraftId -> Text -> Handler ()
postReserveDraftPickR draftId cardToPick = do
    uid <- requireAuthId
    draft <- getDraft draftId
    now <- liftIO getCurrentTime
    runDB $ do
        mres <- selectFirst [PickReservationDrafter ==. uid, PickReservationDraft ==. draftId] [Desc PickReservationNumber]
        let nextpick = case mres of
                Nothing -> 0
                Just (Entity _ res) -> res ^. pickReservationNumber + 1
            thepick = PickReservation draftId nextpick uid (draft ^. draftCube) (CardKey cardToPick) now
        _ <- insert thepick
        return ()
    redirect (ViewDraftR draftId)

postDeleteReserveDraftPickR :: DraftId -> Text -> Handler ()
postDeleteReserveDraftPickR draftId card = do
    userId <- requireAuthId
    runDB $ deleteWhere
        [ PickReservationDraft ==. draftId
        , PickReservationDrafter ==. userId
        , PickReservationCard ==. CardKey card
        ]
    redirect (ViewDraftR draftId)

routeToTextUrl :: Route App -> Handler Text
routeToTextUrl route = withUrlRenderer $ \f -> f route []

checkSendEmail :: DraftId -> Draft -> UserId -> Handler ()
checkSendEmail draftId draft olduid = do
    picks <- getPicks draftId

    mnext <- getNextDrafter (Entity draftId draft)
    case mnext of
        Just newuid | newuid /= olduid -> do
            Just user <- runDB $ get newuid
            let lastpick = Prelude.last picks
                (rnd, _) = pickNumToRC draft (length picks + 1)
            url <- routeToTextUrl (ViewDraftR draftId)
            Just lastpicker <- runDB $ get (lastpick ^. pickDrafter)
            sendEmail user ("Time for draft round " ++ pack (show rnd)) $ [st|
#{pseudonym lastpicker} just drafted #{unCardKey (lastpick ^. pickCard)}.

It is time to make your pick.

#{url}
|]
        _ -> return ()
