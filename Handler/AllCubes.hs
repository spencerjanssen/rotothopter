module Handler.AllCubes where

import Import

getAllCubesR :: Handler Html
getAllCubesR = do
    cubes <- runDB $ selectList ([] :: [Filter Cube]) []
    defaultLayout $ do
        setTitle "All Cubes"
        $(widgetFile "all-cubes")
