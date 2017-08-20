module App.Routes where

import App.Picker as Picker
import App.LaunchDraft as LaunchDraft
import App.ViewDraft as ViewDraft
import Control.Alternative ((<*), (<|>))
import Control.Applicative ((*>))
import Data.Maybe (fromMaybe)
import Prelude (flip, ($), (<$>))
import Pux.Router (int, lit, router, str)

data None a = None

data Route f
    = NotFoundR
    | PickerR Picker.RankId (f Picker.State)
    | LaunchDraftR LaunchDraft.InviteId (f LaunchDraft.State)
    | ViewDraftR ViewDraft.DraftId (f ViewDraft.State)

match :: String -> Route None
match url = fromMaybe NotFoundR $ router url $
    (lit "ranking" *> ((\n -> PickerR (Picker.RankId n) None) <$> int <* lit "view"))
    <|> (lit "draft" *> lit "invite" *> ((\s -> LaunchDraftR (LaunchDraft.InviteId s) None) <$> str))
    <|> (lit "draft" *> ((\n -> ViewDraftR (ViewDraft.DraftId n) None) <$> int) <* lit "view")
