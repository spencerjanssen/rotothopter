module Model.Card where

import ClassyPrelude.Yesod
import Data.Bits
import Database.Persist.Sql

newtype ColorSet = CS Word8
    deriving (Eq, Ord, Show, PersistField, PersistFieldSql)

instance Monoid ColorSet where
    mempty = colorless
    mappend (CS x) (CS y) = CS (x .|. y)

instance FromJSON ColorSet where
    parseJSON x = toColors <$> parseJSON x

colorless, white, blue, black, red, green :: ColorSet
(colorless, white, blue, black, red, green) = (CS 0, p 0, p 1, p 2, p 3, p 4)
 where p = CS . setBit 0

toColor :: Text -> ColorSet
toColor x = case x of
    "White" -> white
    "Blue" -> blue
    "Black" -> black
    "Red" -> red
    "Green" -> green
    _ -> colorless

toColors :: [Text] -> ColorSet
toColors = mconcat . map toColor

newtype TypeSet = TS Word32
    deriving (Eq, Ord, Show, PersistField, PersistFieldSql)

instance Monoid TypeSet where
    mempty = TS 0
    mappend (TS x) (TS y) = TS (x .|. y)

instance FromJSON TypeSet where
    parseJSON x = toTypes <$> parseJSON x

artifact, conspiracy, creature, enchantment, instant, land, phenomenon, plane,
 planeswalker, scheme, sorcery, tribal, vanguard :: TypeSet
(artifact, conspiracy, creature, enchantment, instant, land, phenomenon, plane,
 planeswalker, scheme, sorcery, tribal, vanguard)
 = (p 0, p 1, p 2, p 3, p 4, p 5, p 6, p 7, p 8, p 9, p 10, p 11, p 12)
 where p = TS . setBit 0

toType :: Text -> TypeSet
toType x = case x of
    "Artifact" -> artifact
    "Conspiracy" -> conspiracy
    "Creature" -> creature
    "Enchantment" -> enchantment
    "Instant" -> instant
    "Land" -> land
    "Phenomenon" -> phenomenon
    "Plane" -> plane
    "Planeswalker" -> planeswalker
    "Scheme" -> scheme
    "Sorcery" -> sorcery
    "Tribal" -> tribal
    "Vanguard" -> vanguard
    _ -> mempty

toTypes :: [Text] -> TypeSet
toTypes = mconcat . map toType

typeSubset :: TypeSet -> TypeSet -> Bool
typeSubset (TS x) (TS y) = (x .&. y) == x
