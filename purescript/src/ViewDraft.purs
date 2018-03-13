module App.ViewDraft where

import Prelude (class Eq, bind, eq, id, not, otherwise, pure, show, void, ($), (<>), (==), (<<<))
import Pux (EffModel, noEffects)
import Pux.Html (Html, a, div, h3, li, ol, span, text)
import Pux.Html.Attributes (className, href)
import Pux.Html.Events (onClick)
import Control.Monad.Aff (Aff)
import Network.HTTP.Affjax (AJAX, get, post_)
import Network.HTTP.Affjax.Request (class Requestable)
import Control.Monad.Except (runExcept)
import Data.Array (deleteAt, null)
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error)
import Data.Generic.Rep (class Generic)
import Data.Foreign.Class (class IsForeign, read)
import Data.Foreign.Generic (readGeneric, defaultOptions, toJSONGeneric)
import Data.MediaType.Common (applicationJSON)
import Unsafe.Coerce (unsafeCoerce)
import App.LaunchDraft (Direction(..), applyDirection, directionGlyph, mapIndex, swap)

newtype DraftId = DraftId Int

instance eqDraftId :: Eq DraftId where
    eq (DraftId x) (DraftId y) = x == y

data Action
  = Move Direction Int
  | Delete Int
  | ServerUpdated

data Pender s
  = Ready
  | Waiting
  | NextState s

type State =
  { cards :: Array String
  , picks :: Array Pick
  , userId :: UserId
  , pending :: Pender (Array String)}

newtype Reserved = Reserved (Array String)

derive instance genericReserved :: Generic Reserved _

instance foreignReserved :: IsForeign Reserved where
  read = readGeneric $ defaultOptions {unwrapSingleConstructors = true}

instance requestableReserved :: Requestable Reserved where
  toRequest s = Tuple (Just applicationJSON) (unsafeCoerce $ toJSONGeneric (defaultOptions {unwrapSingleConstructors = true}) s)

newtype UserId = UserId Int

instance eqUserId :: Eq UserId where
  eq (UserId x) (UserId y) = eq x y

derive instance genericUserId :: Generic UserId _

instance foreignUserId :: IsForeign UserId where
  read = readGeneric $ defaultOptions {unwrapSingleConstructors = true}

newtype Pick = Pick
  { card :: String
  , drafter :: UserId
  }

derive instance genericPick :: Generic Pick _

instance foreignCard :: IsForeign Pick where
  read = readGeneric $ defaultOptions {unwrapSingleConstructors = true}

newtype DraftInfo = DraftInfo
  { reservedCards :: Array String
  , picks :: Array Pick
  , userId :: UserId
  }

derive instance genericDraftInfo :: Generic DraftInfo _

instance foreignDraftInfo :: IsForeign DraftInfo where
  read = readGeneric $ defaultOptions {unwrapSingleConstructors = true}

reservedUrl :: DraftId -> String
reservedUrl (DraftId d) = "/draft/" <> show d <> "/reserved"

draftInfoUrl :: DraftId -> String
draftInfoUrl (DraftId d) = "/draft/" <> show d <> "/info"

init :: forall eff. DraftId -> Aff (ajax :: AJAX | eff) State
init draftId = do
  res <- get $ draftInfoUrl draftId
  case runExcept $ read res.response of
    Left e -> throwError $ error $ show e
    Right (DraftInfo s)-> pure {cards: s.reservedCards, picks: s.picks, userId: s.userId, pending: Ready}

updateCards :: forall eff. DraftId -> State -> Array String -> EffModel State Action (ajax :: AJAX | eff)
updateCards draftId oldState newCards
  | oldState.cards == newCards = noEffects oldState
  | otherwise = case oldState.pending of
                  Ready -> {state: oldState {cards = newCards, pending = Waiting}, effects: [postCards draftId newCards]}
                  _ -> noEffects $ oldState {cards = newCards, pending = NextState newCards}

postCards :: forall eff. DraftId -> Array String -> Aff (ajax :: AJAX | eff) Action
postCards draftId cards = do
  void $ post_ (reservedUrl draftId) (Reserved cards)
  pure ServerUpdated

update :: forall eff. DraftId -> Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update draftId ServerUpdated state = case state.pending of
  NextState cards -> {state: state {pending = Waiting}, effects: [postCards draftId cards]}
  _ -> noEffects $ state {pending = Ready}
update draftId (Move d i) state = updateCards draftId state (swap state.cards i (applyDirection d i))
update draftId (Delete i) state = updateCards draftId state $ fromMaybe state.cards $ deleteAt i state.cards

view :: DraftId -> State -> Html Action
view (DraftId _) {cards, picks, userId} | not (null cards) =
    div [] $
        [ h3 [] [text "Reserved Cards"]
        , ol [] $ mapIndex (viewCard picks userId) cards]
view _ _ = div [] []

viewCard :: Array Pick -> UserId -> Int -> String -> Html Action
viewCard picks userId i card =
  li []
    [ dirButton Decrease
    , dirButton Increase
    , deleteButton
    , highlight $ text card]
 where
    highlight = case find (\(Pick pick) -> pick.card == card) picks of
      Nothing -> id
      Just (Pick {drafter})
        | drafter == userId -> span [className "bg-success"] <<< pure
        | otherwise         -> span [className "bg-danger"] <<< pure
    dirButton d = 
        a
            [href "javascript:;", onClick \_ -> Move d i]
            [span [className $ directionGlyph d] []]
    deleteButton =
        a
            [href "javascript:;", onClick \_ -> Delete i]
            [span [className "glyphicon glyphicon-remove"] []]
