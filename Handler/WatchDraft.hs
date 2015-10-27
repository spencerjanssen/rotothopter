module Handler.WatchDraft where

import Import
import Common

import Yesod.EventSource
import Network.Wai.EventSource
import Data.ByteString.Builder

getWatchDraftR :: DraftId -> Handler TypedContent
getWatchDraftR draftId = pollingEventSource Nothing pollFn
 where
    mesg_name = Just "observe_pick"
    pollFn _ Nothing = do
        tc <- subscribeDraftWatcher draftId
        return ([ServerEvent (Just "initialized") Nothing ["initialized"]], Just tc);
    pollFn _ (Just tc) = do
        dp <- atomically $ readTChan tc
        return ([ServerEvent mesg_name Nothing [intDec $ dp ^. pickNumber]], Just tc)
