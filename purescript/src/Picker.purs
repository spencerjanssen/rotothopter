module App.Picker where

import Data.Foreign.Generic
import Data.Array as Array
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Foreign.Class (class IsForeign, read)
import Data.Foreign.Generic.Types (Options)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX, get, post_')
import Prelude (class Eq, Unit, bind, flip, map, otherwise, pure, show, unit, void, ($), (&&), (+), (<$>), (<<<), (<>), (==), (>>=))
import Pux (EffModel, noEffects)
import Pux.Html (Html, a, div, h3, img, li, ol, p, text, ul)
import Pux.Html.Attributes (alt, className, href, key, src, style)
import Pux.Html.Events (onClick)

data Action
  = AddRanking Ranking
  | RankingReceived

newtype RankId = RankId Int

instance eqRankId :: Eq RankId where
    eq (RankId x) (RankId y) = x == y

type Card = String

genericOptions :: Options
genericOptions = defaultOptions {unwrapSingleConstructors = true}

newtype Ranking = Ranking {better :: Card, worse :: Card}

derive instance genericRanking :: Generic Ranking _

instance foreignRanking :: IsForeign Ranking where
  read = readGeneric genericOptions

newtype State = State
  { cards :: Array String
  , rankings :: Array Ranking
  }

derive instance genericState :: Generic State _

instance foreignState :: IsForeign State where
  read = readGeneric genericOptions

init :: RankId -> Aff _ State
init (RankId r) = do
  res <- get $ "/ranking/" <> show r
  case runExcept $ read res.response of
    Left e -> throwError $ error $ show e
    Right s -> pure s

submitRanking :: forall eff. RankId -> Ranking -> Aff (ajax :: AJAX | eff) Action
submitRanking (RankId r) (Ranking {better, worse}) = do
  void $ post_' ("/ranking/" <> show r <> "/choice/" <> better <> "/" <> worse) (Nothing :: Maybe Unit)
  pure RankingReceived

update :: RankId -> Action -> State -> EffModel State Action _
update rid (AddRanking r) (State state)
  = {state: State $ state { rankings = Array.snoc state.rankings r }, effects: [submitRanking rid r]}
update rid RankingReceived state = noEffects state

view :: RankId -> State -> Html Action
view _ (State state) =
  div [] $
    [ h3 [] [text "Pack 1, pick 1, which do you take?"] ]
    <> choice true mlr
    <> case mlr of
        Nothing -> []
        Just {left, right} ->
          choice false ((applyFact (mkRanker (Ranking {better: left, worse: right})) <$> mt) >>= dropTop >>= firstComp)
          <> choice false ((applyFact (mkRanker (Ranking {better: right, worse: left})) <$> mt) >>= dropTop >>= firstComp)
    <> case best of
        Nil -> [h3 [] [text $ show clicks <> " clicks until your top rated card!"]]
        xs ->
          [ h3 [] [text $ "Top cards (" <> show clicks <> " clicks until the next rating):"]
          , ol [] (viewCardList xs)]
 where
  choice visible (Just {left, right}) =
    [div
      [ className "row"
      , key $ "card-fight-" <> left <> "-vs-" <> right
      , style $ if visible then [] else [Tuple "display" "none"]
      ]
      [ card left right, card right left]]
  choice _ Nothing = []
  card better worse =
    div [className "col-md-3 col-xs-5"]
        [ a
          [ href "javascript:;"
          , onClick \_ -> AddRanking (Ranking {better, worse})
          , alt better]
          [img [key ("card-img-better" <> better), className "img-responsive", src $ cardImageURL better] []]]
  mt = applyFact (mkRankers state.rankings) <$> mkMT (fromFoldable state.cards)
  clicks = maybe 0 clicksTilTop (mt >>= dropTop)
  best = maybe Nil knownTop mt
  mlr = mt >>= dropTop >>= firstComp

cardImageURL :: String -> String
cardImageURL c = "http://gatherer.wizards.com/Handlers/Image.ashx?type=card&name=" <> c

viewCardList :: forall a. List Card -> Array (Html a)
viewCardList cs = Array.fromFoldable $ map (\c -> li [] [text c]) cs

data MT a
    = Top a (Maybe (MT a))
    | Merge (MT a) (MT a)

type Query a = {left :: a, right :: a}

data RankResult = Better | Worse

type Ranker m a = a -> a -> m RankResult

mkRanker :: Ranking -> Ranker Maybe Card
mkRanker (Ranking {better, worse}) a b
 | a == better && b == worse = Just Better
 | b == better && a == worse = Just Worse
 | otherwise = Nothing

mkRankers :: Array Ranking -> Ranker Maybe Card
mkRankers = foldr (\r f -> \a b -> mkRanker r a b <|> f a b) (\_ _ -> Nothing)

mkMT :: forall a. List a -> Maybe (MT a)
mkMT xs = pairwise Merge $ map (\x -> Top x Nothing) xs

pairwise :: forall a. (a -> a -> a) -> List a -> Maybe a
pairwise = go Nil
 where
  go Nil _ Nil = Nothing
  go (Cons x Nil) _ Nil = Just x
  go acc f (Cons x (Cons y xs)) = go (Cons (f x y) acc) f xs
  go acc f xs = go Nil (flip f) (xs <> acc)

clicksTilTop :: forall a. MT a -> Int
clicksTilTop (Top _ _) = 0
clicksTilTop (Merge t u) = 1 + clicksTilTop t + clicksTilTop u

dropTop :: forall a. MT a -> Maybe (MT a)
dropTop (Top _ mt) = mt >>= dropTop
dropTop t = Just t

firstComp :: forall a. MT a -> Maybe (Query a)
firstComp (Top x t) = Nothing
firstComp (Merge (Top x _) (Top y _)) = Just {left: x, right: y}
firstComp (Merge t u) = firstComp t <|> firstComp u

knownTop :: forall a. MT a -> List a
knownTop (Top x mt) = Cons x $ maybe Nil knownTop mt
knownTop (Merge _ _) = Nil

applyFact :: forall a. Ranker Maybe a -> MT a -> MT a
applyFact rank (Top x mt) = Top x (applyFact rank <$> mt)
applyFact rank (Merge t u) = case Tuple (applyFact rank t) (applyFact rank u) of
    Tuple (Top x mt') (Top y mu') -> case rank x y of
        Just Better -> Top x (mergeMT' rank mt' (Just (Top y mu')))
        Just Worse -> Top y (mergeMT' rank (Just (Top x mt')) mu')
        Nothing -> Merge (Top x mt') (Top y mu')
    Tuple t' u' -> Merge t' u'

mergeMT' :: forall a. Ranker Maybe a -> Maybe (MT a) -> Maybe (MT a) -> Maybe (MT a)
mergeMT' rank (Just (Top x mt)) (Just (Top y mu)) = Just $ case rank x y of
    Nothing -> Merge (Top x mt) (Top y mu)
    Just Better -> Top x (mergeMT' rank mt (Just (Top y mu)))
    Just Worse -> Top y (mergeMT' rank (Just (Top x mt)) mu)
mergeMT' _ mt mu = mergeMT mt mu

mergeMT :: forall a. Maybe (MT a) -> Maybe (MT a) -> Maybe (MT a)
mergeMT (Just t) (Just u) = Just (Merge t u)
mergeMT Nothing mu = mu
mergeMT mt Nothing = mt
