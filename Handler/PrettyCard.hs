module Handler.PrettyCard where

import Import
import qualified Data.Map as Map

maybeCardInfo :: Text -> Handler (Maybe Card)
maybeCardInfo cname = fmap (fmap entityVal) $ runDB $ getBy (UniqueCardName cname)

cardImgUrl :: Card -> Text
cardImgUrl c = base ++ c ^. cardCard
 where
    base = "http://gatherer.wizards.com/Handlers/Image.ashx?type=card&name="

colorBadgeMap :: Map ColorSet StaticRoute
colorBadgeMap = Map.fromList
    [ (white, img_mana_15_w_png)
    , (blue, img_mana_15_u_png)
    , (black, img_mana_15_b_png)
    , (red ,img_mana_15_r_png)
    , (green,img_mana_15_g_png)
    , (black++green,img_mana_15_bg_png)
    , (black++red,img_mana_15_br_png)
    , (black++blue,img_mana_15_ub_png)
    , (blue++red,img_mana_15_ur_png)
    , (blue++green,img_mana_15_gu_png)
    , (green++white,img_mana_15_gw_png)
    , (green++red,img_mana_15_rg_png)
    , (red++white,img_mana_15_rw_png)
    , (black++white,img_mana_15_wb_png)
    , (blue++white,img_mana_15_wu_png)
    ]

colorBadge :: Card -> Route App
colorBadge card = StaticR $ case Map.lookup (card ^. cardColors) colorBadgeMap of
    Just cb -> cb
    Nothing | card ^. cardColors == colorless -> if land `typeSubset` (card ^. cardTypes)
                                                    then img_mana_15_tap_png
                                                    else img_mana_15_0_png
            | otherwise -> img_mana_15_snow_png

prettyCard :: Card -> Maybe Text -> WidgetT App IO ()
prettyCard card mlinkClass = $(widgetFile "inline-card")
 where linkClass = fromMaybe "" mlinkClass

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

cardCategoryGlyph :: CardCategory -> Maybe (Route App)
cardCategoryGlyph cat = StaticR <$> case cat of
    White -> Just img_mana_40_w_png
    Blue -> Just img_mana_40_u_png
    Black -> Just img_mana_40_b_png
    Red -> Just img_mana_40_r_png
    Green -> Just img_mana_40_g_png
    Land -> Just img_mana_40_tap_png
    ArtifactColorless -> Just img_mana_40_0_png
    _ -> Nothing

categorize :: Card -> CardCategory
categorize card
    | land `typeSubset` types = Land
    | colors == white = White
    | colors == blue = Blue
    | colors == black = Black
    | colors == red = Red
    | colors == green = Green
    | colors == colorless = ArtifactColorless
    | otherwise = Multicolor
 where
    types = card ^. cardTypes
    colors = card ^. cardColors

categorizeCardList :: [Card] -> [(CardCategory, [Card])]
categorizeCardList cs = Map.toList $ Map.fromListWith (flip (++)) [(categorize c, [c]) | c <- cs]

cardListView :: [Card] -> Text -> Maybe (Text -> WidgetT App IO ()) -> WidgetT App IO ()
cardListView cs clss mtrans = do
    let catcards = map (sortBy (comparing _cardCard)) <$> categorizeCardList cs
    $(widgetFile "card-list")
