module Model.Card where

import ClassyPrelude.Yesod
import Data.Bits
import Database.Persist.Sql

newtype ColorSet = CS Word8
    deriving (Eq, Ord, Show, PersistField, PersistFieldSql, Generic, NFData)

instance Semigroup ColorSet where
    (<>) = mappend

instance Monoid ColorSet where
    mempty = colorless
    mappend (CS x) (CS y) = CS (x .|. y)

instance FromJSON ColorSet where
    parseJSON x = toColors <$> parseJSON x

instance ToJSON ColorSet where
    toJSON = toJSON . fromColors

colorless, white, blue, black, red, green :: ColorSet
(colorless, white, blue, black, red, green) = (CS 0, p 0, p 1, p 2, p 3, p 4)
  where
    p = CS . setBit 0

toColor :: Text -> ColorSet
toColor x = fromMaybe colorless $ lookup x colorMap

colorMap :: [(Text, ColorSet)]
colorMap =
    [ ("Colorless", colorless)
    , ("W", white)
    , ("U", blue)
    , ("B", black)
    , ("R", red)
    , ("G", green)
    ]

toColors :: [Text] -> ColorSet
toColors = mconcat . map toColor

fromColors :: ColorSet -> [Text]
fromColors (CS x) = [color | (color, CS y) <- colorMap, x .&. y /= 0]

newtype TypeSet = TS Word32
    deriving (Eq, Ord, Show, PersistField, PersistFieldSql, Generic, NFData)

instance Semigroup TypeSet where
    (<>) = mappend

instance Monoid TypeSet where
    mempty = TS 0
    mappend (TS x) (TS y) = TS (x .|. y)

instance FromJSON TypeSet where
    parseJSON x = toTypes <$> parseJSON x

instance ToJSON TypeSet where
    toJSON = toJSON . fromTypes

artifact
    , conspiracy
    , creature
    , enchantment
    , instant
    , land
    , phenomenon
    , plane
    , planeswalker
    , scheme
    , sorcery
    , tribal
    , vanguard ::
        TypeSet
( artifact
    , conspiracy
    , creature
    , enchantment
    , instant
    , land
    , phenomenon
    , plane
    , planeswalker
    , scheme
    , sorcery
    , tribal
    , vanguard
    ) =
        (p 0, p 1, p 2, p 3, p 4, p 5, p 6, p 7, p 8, p 9, p 10, p 11, p 12)
      where
        p = TS . setBit 0

toType :: Text -> TypeSet
toType x = fromMaybe mempty $ lookup x typeMap

typeMap :: [(Text, TypeSet)]
typeMap =
    [ ("Artifact", artifact)
    , ("Conspiracy", conspiracy)
    , ("Creature", creature)
    , ("Enchantment", enchantment)
    , ("Instant", instant)
    , ("Land", land)
    , ("Phenomenon", phenomenon)
    , ("Plane", plane)
    , ("Planeswalker", planeswalker)
    , ("Scheme", scheme)
    , ("Sorcery", sorcery)
    , ("Tribal", tribal)
    , ("Vanguard", vanguard)
    ]

toTypes :: [Text] -> TypeSet
toTypes = mconcat . map toType

typeSubset :: TypeSet -> TypeSet -> Bool
typeSubset (TS x) (TS y) = (x .&. y) == x

fromTypes :: TypeSet -> [Text]
fromTypes ts = [typeName | (typeName, tm) <- typeMap, typeSubset tm ts]
