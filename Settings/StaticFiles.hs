module Settings.StaticFiles where

import Settings     (appStaticDir, compileTimeAppSettings)
import Yesod.Static (staticFiles)
import qualified Yesod.Static as Static
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (liftData)
import Prelude (String, Maybe(..), return, (=<<), (++), error)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Aeson as Aeson
import Data.ByteString.Lazy as BS
import Data.String (fromString)

-- This generates easy references to files in the static directory at compile time,
-- giving you compile-time verification that referenced files exist.
-- Warning: any files added to your static directory during run-time can't be
-- accessed this way. You'll have to use their FilePath or URL to access them.
--
-- For example, to refer to @static/js/script.js@ via an identifier, you'd use:
--
--     js_script_js
--
-- If the identifier is not available, you may use:
--
--     StaticFile ["js", "script.js"] []
staticFiles (appStaticDir compileTimeAppSettings)

manifest_json :: Map String String
manifest_json = $(liftData =<< runIO (do
    bytes <- BS.readFile (appStaticDir compileTimeAppSettings ++ "/gen/manifest.json")
    Just manifest <- return (Aeson.decode bytes)
    return (manifest :: Map String String)))

gen_main_hash_js :: Static.Route Static.Static
gen_main_hash_js = case Map.lookup "main.js" manifest_json of
    Just p -> Static.StaticRoute ["gen", fromString p] []
    _ -> error "main.js missing from manifest"
