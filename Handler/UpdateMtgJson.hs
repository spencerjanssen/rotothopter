module Handler.UpdateMtgJson where

import Data.Aeson
import qualified Data.HashMap.Strict as Map
import Import hiding (httpLbs)
import Network.HTTP.Client (httpLbs)

getUpdateMtgJsonR :: Handler Html
getUpdateMtgJsonR = do
    defaultLayout
        [whamlet|
            <form method=post action=@{UpdateMtgJsonR}>
                <button .btn btn-primary type="submit">
                    Reload mtgjson.com data
        |]

-- Possible values: normal, split, flip, double-faced, token, plane, scheme, phenomenon, leveler, vanguard
data Layout
    = Split
    | Aftermath
    | Flip
    | DoubleFaced
    | MehLayout
    deriving (Show)

data CardInfo = CardInfo {ciname :: Text, cicolors :: ColorSet, citypes :: TypeSet, layout :: Layout, names :: Maybe [Text]}
    deriving (Show)

instance FromJSON CardInfo where
    parseJSON (Object v) =
        CardInfo
            <$> v .: "name"
            <*> v .:? "colors" .!= mempty
            <*> v .:? "types" .!= mempty
            <*> v .: "layout"
            <*> ((nullIsNothing =<<) <$> v .:? "names" .!= Nothing)
      where
        nullIsNothing [] = Nothing
        nullIsNothing xs = Just xs
    parseJSON _ = mzero

instance FromJSON Layout where
    parseJSON (String s) = return $
        case s of
            "split" -> Split
            "aftermath" -> Aftermath
            "flip" -> Flip
            "double-faced" -> DoubleFaced
            _ -> MehLayout
    parseJSON _ = mzero

splitLike :: Layout -> Bool
splitLike Split = True
splitLike Aftermath = True
splitLike _ = False

-- | Assume all CardInfo are part of the same split card
combineCardInfo :: [CardInfo] -> CardInfo
combineCardInfo xs@(CardInfo{layout = l, names = Just ns} : _)
    | splitLike l =
        CardInfo
            { ciname = name
            , cicolors = foldMap cicolors xs
            , citypes = foldMap citypes xs
            , layout = l
            , names = Just ns
            }
  where
    name = intercalate " // " ns
combineCardInfo xs@(CardInfo{names = Just (n : _)} : _) = prime
  where
    (prime : _) = [x | x <- xs, ciname x == n]
combineCardInfo xs = error $ "called with bad argument: " ++ show xs

massage :: HashMap Text CardInfo -> [Card]
massage cs' =
    [ Card (ciname ci) (cicolors ci) (citypes ci)
    | ci <- singlenames ++ massagedMultinames
    ]
  where
    cs = Map.elems cs'
    singlenames = [c | c@(CardInfo{names = Nothing}) <- cs]
    multinames =
        Map.elems $
            Map.fromListWith
                (++)
                [(ns, [c]) | c@(CardInfo{names = Just ns}) <- cs]
    massagedMultinames = map combineCardInfo multinames

fetchAllCards :: Handler (Either String (HashMap Text CardInfo))
fetchAllCards = do
    req <- parseUrlThrow url
    manager <- getHttpManager <$> getYesod
    eitherDecode . responseBody <$> liftIO (httpLbs req manager)
  where
    url = "http://mtgjson.com/json/AllCards.json"

postUpdateMtgJsonR :: Handler Html
postUpdateMtgJsonR = do
    allCards <- fetchAllCards
    case massage <$> allCards of
        Left err -> fail err
        Right cs -> do
            forM_ (batches 100 cs) $ \cs' ->
                runDB $
                    forM_ cs' $ \c ->
                        void $ upsert c (cardToUpdates c)
            defaultLayout
                [whamlet|
                <p>Successfully loaded #{length cs} cards
            |]

batches :: Int -> [a] -> [[a]]
batches _ [] = []
batches n xs = case splitAt n xs of
    (ls, rs) -> ls : batches n rs

cardToUpdates :: Card -> [Update Card]
cardToUpdates c =
    [ CardColors =. c ^. cardColors
    , CardTypes =. c ^. cardTypes
    ]
