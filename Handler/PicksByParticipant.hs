module Handler.PicksByParticipant where

import Common
import qualified Database.Esqueleto as E
import Handler.PrettyCard
import Import

-- TODO, links to download list as txt/mtgo deck/etc?
getPicksByParticipantR :: DraftId -> UserId -> Handler Html
getPicksByParticipantR draftId userId = do
    cards <- sortBy (comparing (view cardCard)) <$> getParticipantPicks draftId userId
    usr <- runDB $ get404 userId
    defaultLayout $ do
        setTitle "Picks by color"
        $(widgetFile "picks-by-participant")

getParticipantPicks :: DraftId -> UserId -> Handler [Card]
getParticipantPicks did uid = map snd <$> getPicksAndInfo did (Just expr)
  where
    expr pick _ = pick E.^. PickDrafter E.==. E.val uid
