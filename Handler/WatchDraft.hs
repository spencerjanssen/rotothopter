module Handler.WatchDraft (getWatchDraftR) where

import Common
import Import

import Control.Concurrent.STM.Delay
import Data.ByteString.Builder
import Network.Wai.EventSource
import Yesod.EventSource

getWatchDraftR :: DraftId -> Int -> Handler TypedContent
getWatchDraftR draftId seenPicks = pollingEventSource Nothing pollFn
  where
    mesg_name = Just "observe_pick"
    waitMicroseconds = 18 * 10 ^ (7 :: Int) -- 3 minutes
    pollFn _ Nothing = do
        watcher <- getDraftWatcher draftId
        return ([ServerEvent (Just "initialized") Nothing ["initialized"]], Just (watcher, seenPicks))
    pollFn _ (Just (watcher, lastCount)) = do
        delay <- liftIO $ newDelay waitMicroseconds
        let tryPick = Right <$> waitForPick watcher lastCount
            tryDelay = Left <$> waitDelay delay
        result <- atomically $ tryPick `orElseSTM` tryDelay
        liftIO $ cancelDelay delay
        return $ case result of
            Left _ -> ([CommentEvent "keepalive"], Just (watcher, lastCount))
            Right newCount -> ([ServerEvent mesg_name Nothing [intDec newCount]], Just (watcher, newCount))
