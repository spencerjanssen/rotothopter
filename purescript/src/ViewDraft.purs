module App.ViewDraft where

import Prelude (class Eq, bind, not, otherwise, pure, show, void, ($), (<>), (==))
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
  , pending :: Pender (Array String)}

newtype ServerState = ServerState (Array String)

derive instance genericServerState :: Generic ServerState _

instance foreignState :: IsForeign ServerState where
  read = readGeneric $ defaultOptions {unwrapSingleConstructors = true}

instance requestableServerState :: Requestable ServerState where
  toRequest s = Tuple (Just applicationJSON) (unsafeCoerce $ toJSONGeneric (defaultOptions {unwrapSingleConstructors = true}) s)

reservedUrl :: DraftId -> String
reservedUrl (DraftId d) = "/draft/" <> show d <> "/reserved"

init :: forall eff. DraftId -> Aff (ajax :: AJAX | eff) State
init draftId = do
  res <- get $ reservedUrl draftId
  case runExcept $ read res.response of
    Left e -> throwError $ error $ show e
    Right (ServerState s)-> pure {cards: s, pending: Ready}

updateCards :: forall eff. DraftId -> State -> Array String -> EffModel State Action (ajax :: AJAX | eff)
updateCards draftId oldState newCards
  | oldState.cards == newCards = noEffects oldState
  | otherwise = case oldState.pending of
                  Ready -> {state: {cards: newCards, pending: Waiting}, effects: [postCards draftId newCards]}
                  _ -> noEffects {cards: newCards, pending: NextState newCards}

postCards :: forall eff. DraftId -> Array String -> Aff (ajax :: AJAX | eff) Action
postCards draftId cards = do
  void $ post_ (reservedUrl draftId) (ServerState cards)
  pure ServerUpdated

update :: forall eff. DraftId -> Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update draftId ServerUpdated state = case state.pending of
  NextState cards -> {state: state {pending = Waiting}, effects: [postCards draftId cards]}
  _ -> noEffects $ state {pending = Ready}
update draftId (Move d i) state = updateCards draftId state (swap state.cards i (applyDirection d i))
update draftId (Delete i) state = updateCards draftId state $ fromMaybe state.cards $ deleteAt i state.cards

view :: DraftId -> State -> Html Action
view (DraftId _) {cards} | not (null cards) =
    div [] $
        [ h3 [] [text "Reserved Cards"]
        , ol [] $ mapIndex viewCard cards]
view _ _ = div [] []

viewCard :: Int -> String -> Html Action
viewCard i card = li [] [dirButton Decrease, dirButton Increase, deleteButton, text card]
 where
    dirButton d =
        a
            [href "javascript:;", onClick \_ -> Move d i]
            [span [className $ directionGlyph d] []]
    deleteButton =
        a
            [href "javascript:;", onClick \_ -> Delete i]
            [span [className "glyphicon glyphicon-remove"] []]
