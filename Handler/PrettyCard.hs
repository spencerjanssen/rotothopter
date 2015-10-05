module Handler.PrettyCard where

import Import
import qualified Data.Map as Map

maybeCardInfo cname = fmap (fmap entityVal) $ runDB $ getBy (CardName cname)

cardImgUrl :: Card -> Text
cardImgUrl c = base ++ cardCardName c
 where
    base = "http://gatherer.wizards.com/Handlers/Image.ashx?type=card&name="

-- colorBadge :: Card -> Text
colorBadge card = StaticR $ case sort $ cardCardColors card of
    ["White"] -> img_mana_15_w_png
    ["Blue"] -> img_mana_15_u_png
    ["Black"] -> img_mana_15_b_png
    ["Red"] -> img_mana_15_r_png
    ["Green"] -> img_mana_15_g_png
    -- handle multicolor later:
    [c1, c2] -> case (c1, c2) of
        ("Black", "Green") -> img_mana_15_bg_png
        ("Black", "Red") -> img_mana_15_br_png
        ("Black", "Blue") -> img_mana_15_ub_png
        ("Blue", "Red") -> img_mana_15_ur_png
        ("Blue", "Green") -> img_mana_15_gu_png
        ("Green", "White") -> img_mana_15_gw_png
        ("Green", "Red") -> img_mana_15_rg_png
        ("Red", "White") -> img_mana_15_rw_png
        ("Black", "White") -> img_mana_15_wb_png
        ("Blue", "White") -> img_mana_15_wu_png
        _ -> img_mana_15_snow_png
    [] -> if "Land" `elem` cardCardTypes card
            then img_mana_15_tap_png
            else img_mana_15_0_png
    _ -> img_mana_15_snow_png

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

cardCategoryGlyph cat = StaticR <$> case cat of
    White -> Just img_mana_40_w_png
    Blue -> Just img_mana_40_u_png
    Black -> Just img_mana_40_b_png
    Red -> Just img_mana_40_r_png
    Green -> Just img_mana_40_g_png
    Land -> Just img_mana_40_tap_png
    ArtifactColorless -> Just img_mana_40_0_png
    _ -> Nothing

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
