module Handler.PrettyCard where

import Import
import qualified Data.Map as Map

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

data CardCategory
    = Unknown
    | White
    | Blue
    | Black
    | Red
    | Green
    | Multicolor
    | Land
    | ArtifactColorless
 deriving (Eq, Ord, Show)

categorize :: Either Text Card -> CardCategory
categorize (Left _) = Unknown
categorize (Right card) = case colors of
    _ | "Land" `elem` types -> Land
    ["White"] -> White
    ["Blue"] -> Blue
    ["Black"] -> Black
    ["Red"] -> Red
    ["Green"] -> Green
    (_:_:_) -> Multicolor
    _ -> ArtifactColorless
 where
    types = cardCardTypes card
    colors = cardCardColors card

categorizeCardList :: [Either Text Card] -> [(CardCategory, [Either Text Card])]
categorizeCardList cs = Map.toList $ Map.fromListWith (flip (++)) [(categorize c, [c]) | c <- cs]
-- we flip there to preserve the input order

categorizeUnknownCardList :: [Text] -> Handler [(CardCategory, [Either Text Card])]
categorizeUnknownCardList ts =
    categorizeCardList <$> forM ts (\t -> maybe (Left t) (Right) <$> maybeCardInfo t)
