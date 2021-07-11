module Handler.MakeDraftPick where

import Common
import Handler.PrettyCard
import Import
import Import.Mail
import Text.Shakespeare.Text
import qualified Prelude (last)

getMakeDraftPickR :: DraftId -> Text -> Handler Html
getMakeDraftPickR draftId cardToPick = do
    uid <- requireAuthId
    draft <- getDraft draftId
    drafters <- map entityKey <$> getParticipants draftId
    when (uid `onotElem` drafters) $ fail "you are not in this draft"
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
        Just uid'
            | uid /= uid' -> fail "it isn't your turn to pick yet"
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
            void $ getDraftWatcher draftId
            redirect (ViewDraftR draftId)
        else fail "you can't pick that card"

postReserveDraftPickR :: DraftId -> Text -> Handler ()
postReserveDraftPickR draftId cardToPick = do
    uid <- requireAuthId
    draft <- getDraft draftId
    runDB $ do
        mres <- selectFirst [PickReservationDrafter ==. uid, PickReservationDraft ==. draftId] [Desc PickReservationNumber]
        let nextpick = case mres of
                Nothing -> 0
                Just (Entity _ res) -> res ^. pickReservationNumber + 1
            thepick = PickReservation draftId nextpick uid (draft ^. draftCube) (CardKey cardToPick)
        _ <- insert thepick
        return ()
    redirect (ViewDraftR draftId)

postReservedCardsR :: DraftId -> Handler ()
postReservedCardsR draftId = do
    userId <- requireAuthId
    cards <- requireCheckJsonBody
    runDB $ do
        cubeId <- _draftCube <$> get404 draftId
        deleteWhere [PickReservationDraft ==. draftId, PickReservationDrafter ==. userId]
        let row i card = PickReservation draftId i userId cubeId card
        insertMany_ $ zipWith row [0 ..] cards

postDeleteReserveDraftPickR :: DraftId -> Text -> Handler ()
postDeleteReserveDraftPickR draftId card = do
    userId <- requireAuthId
    runDB $
        deleteWhere
            [ PickReservationDraft ==. draftId
            , PickReservationDrafter ==. userId
            , PickReservationCard ==. CardKey card
            ]
    redirect (ViewDraftR draftId)

postForceNextReservedPickR :: DraftId -> Handler ()
postForceNextReservedPickR draftId = do
    True <- isCommissioner draftId
    draft <- getDraft draftId
    mnext <- getNextDrafter (Entity draftId draft)
    picks <- getPicks draftId
    allowedCards <- getPickAllowedCards draftId draft
    case mnext of
        Nothing -> return ()
        Just nextUid -> do
            reserveds <-
                runDB $
                    selectList
                        [ PickReservationDrafter ==. nextUid
                        , PickReservationDraft ==. draftId
                        ]
                        [Asc PickReservationNumber]
            case filter (`oelem` allowedCards) $ map (unCardKey . _pickReservationCard . entityVal) reserveds of
                (res : _) -> actualPostMakeDraftPickR draftId nextUid picks draft res
                _ -> return ()
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
            sendEmail user ("Time for draft round " ++ pack (show $ succ rnd)) $
                [st|
#{pseudonym lastpicker} just drafted #{unCardKey (lastpick ^. pickCard)}.

It is time to make your pick.

#{url}
|]
        _ -> return ()
