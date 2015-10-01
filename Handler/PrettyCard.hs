module Handler.PrettyCard where

import Import

maybeCardInfo cname = fmap (fmap entityVal) $ runDB $ getBy (CardName cname)

cardImgUrl :: Card -> Text
cardImgUrl c = base ++ cardCardName c
 where
    base = "http://gatherer.wizards.com/Handlers/Image.ashx?type=card&name="

colorBadge :: Card -> Text
colorBadge card = badge $ case sort $ cardCardColors card of
    [] -> "x"
    ["White"] -> "w"
    ["Blue"] -> "u"
    ["Black"] -> "b"
    ["Red"] -> "r"
    ["Green"] -> "g"
    -- handle multicolor later:
    _ -> ""
 where
    badge s = "http://mtgjson.com/images/" ++ s ++ ".png"

prettyCard cname = do
    mcard <- handlerToWidget $ maybeCardInfo cname
    $(widgetFile "inline-card")
