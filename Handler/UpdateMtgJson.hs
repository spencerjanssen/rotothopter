module Handler.UpdateMtgJson where

import Data.Aeson
import qualified Data.HashMap.Strict as Map
import qualified Data.List.NonEmpty as NE
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

newtype AtomicCards = AtomicCards (HashMap Text (NE.NonEmpty CardInfo))
    deriving (Show)

instance FromJSON AtomicCards where
    parseJSON = withObject "AtomicCards" $ \v -> AtomicCards <$> v .: "data"

-- Possible values: normal, split, flip, double-faced, token, plane, scheme, phenomenon, leveler, vanguard
data Layout
    = Split
    | Aftermath
    | Flip
    | DoubleFaced
    | MehLayout
    deriving (Show)

data CardInfo = CardInfo {ciname :: Text, cifacename :: Maybe Text, cicolors :: ColorSet, citypes :: TypeSet, layout :: Layout}
    deriving (Show)

instance FromJSON CardInfo where
    parseJSON (Object v) =
        CardInfo
            <$> v .: "name"
            <*> v .:? "faceName"
            <*> v .:? "colors" .!= mempty
            <*> v .:? "types" .!= mempty
            <*> v .: "layout"
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

flattenAtomicCards :: AtomicCards -> [Card]
flattenAtomicCards (AtomicCards cs) = map (uncurry flattenNameGroup) $ Map.toList cs

flattenNameGroup :: Text -> NE.NonEmpty CardInfo -> Card
flattenNameGroup groupName cis@(prime NE.:| _)
    | splitLike $ layout prime = 
        Card
            { _cardCard = groupName
            , _cardColors = foldMap cicolors cis
            , _cardTypes = foldMap citypes cis
            }
    | otherwise = 
        Card
            { _cardCard = fromMaybe (ciname prime) $ cifacename prime
            , _cardColors = cicolors prime
            , _cardTypes = citypes prime
            }

fetchAllCards :: Handler (Either String AtomicCards)
fetchAllCards = do
    req <- parseUrlThrow url
    manager <- getHttpManager <$> getYesod
    eitherDecode . responseBody <$> liftIO (httpLbs req manager)
  where
    url = "https://mtgjson.com/api/v5/AtomicCards.json"

postUpdateMtgJsonR :: Handler Html
postUpdateMtgJsonR = do
    allCards <- fetchAllCards
    case flattenAtomicCards <$> allCards of
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
