module Handler.UpdateMtgJson where

import Import
import Data.Aeson
import qualified Data.Map as Map

getUpdateMtgJsonR :: Handler Html
getUpdateMtgJsonR = do
    defaultLayout
        [whamlet|
            <form method=post action=@{UpdateMtgJsonR}>
                <button .btn btn-primary type="submit">
                    Reload mtgjson.com data
        |]

data CardInfo = CardInfo { cicolors :: [Text], citypes :: [Text] }

instance FromJSON CardInfo where
    parseJSON (Object v) = CardInfo
                            <$> v .:? "colors" .!= []
                            <*> v .:? "types" .!= []
    parseJSON _ = mzero

massage :: Map Text CardInfo -> [Card]
massage = map (\(n, i) -> Card n (cicolors i) (citypes i)) . Map.toList

postUpdateMtgJsonR :: Handler Html
postUpdateMtgJsonR = do
    req <- parseUrl url
    js <- responseBody <$> httpLbs req
    case massage <$> eitherDecode js of
        Left err -> fail err
        Right cs -> do
            runDB $ do
                deleteWhere ([] :: [Filter Card])
                mapM_ (\c -> insert c >> return ()) cs
            defaultLayout [whamlet|
                <p>Successfully loaded #{length cs} cards
            |]
 where
    url = "http://mtgjson.com/json/AllCards.json"
