module Handler.WatchDraft where

import Import
import Common

import Yesod.EventSource
import Network.Wai.EventSource
import Data.ByteString.Builder

getWatchDraftR :: DraftId -> Handler TypedContent
getWatchDraftR draftId = do
    tc <- subscribeDraftWatcher draftId
    pollingEventSource () $ \_ () -> do
        dp <- atomically $ readTChan tc
        return ([ServerEvent mesg_name Nothing [intDec $ draftPickPickNumber dp]], ())
 where
    mesg_name = Just "observe_pick"
