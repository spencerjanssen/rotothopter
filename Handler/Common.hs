-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR =
    return $
        TypedContent "image/png" $
            toContent $(embedFile "config/favicon.png")

getFaviconAttnR :: Handler TypedContent
getFaviconAttnR =
    return $
        TypedContent "image/png" $
            toContent $(embedFile "config/favicon-attn.png")

getRobotsR :: Handler TypedContent
getRobotsR =
    return $
        TypedContent typePlain $
            toContent $(embedFile "config/robots.txt")
