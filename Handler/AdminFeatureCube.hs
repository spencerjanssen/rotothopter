module Handler.AdminFeatureCube where

import Import

postAdminFeatureCubeR :: CubeId -> Bool -> Handler ()
postAdminFeatureCubeR cubeId False = do
    runDB $ deleteWhere [FeaturedCubeCube ==. cubeId]
    redirect (ViewCubeListR cubeId)
postAdminFeatureCubeR cubeId True = do
    now <- liftIO getCurrentTime
    void $ runDB $ upsert (FeaturedCube cubeId now) [FeaturedCubeCreated =. now]
    redirect (ViewCubeListR cubeId)
