module Model.InviteHash (InviteHash, newInviteHash) where

import ClassyPrelude.Yesod
import Database.Persist.Sql
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)

newtype InviteHash = IH Text
    deriving (Eq, Ord, Show, PersistField, PersistFieldSql)

newInviteHash :: MonadIO m => m InviteHash
newInviteHash = liftIO (IH . toText <$> nextRandom)
