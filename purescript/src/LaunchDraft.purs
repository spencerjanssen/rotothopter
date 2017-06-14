module App.LaunchDraft where

import Prelude (class Eq, (==), (<$>), ($), (<>), (-), (+), bind, pure, show)
import Pux.Html (Html, a, button, div, h2, img, li, ol, span, p, text, ul, input, form)
import Pux.Html.Attributes (type_, value, name, action, encType, method, className, href)
import Pux.Html.Events (onClick)
import Control.Monad.Aff (Aff)

newtype InviteId = InviteId String

instance eqInviteId :: Eq InviteId where
    eq (InviteId x) (InviteId y) = x == y

newtype UserId = UserId Int

type User =
    { uid :: UserId
    , name :: String
    }

type State =
    { users :: Array User }

data Direction
    = Decrease
    | Increase

data Action
    = Move Direction Int

init :: InviteId -> Aff _ State
init _ = do
    users <- seedUsers
    pure {users}

foreign import seedUsers :: forall e. Aff e (Array User)

update :: Action -> State -> State
update (Move d i) state = state {users = swap state.users i (applyDirection d i)}

applyDirection :: Direction -> Int -> Int
applyDirection Increase = (_+1)
applyDirection Decrease = (_-1)

directionGlyph :: Direction -> String
directionGlyph Increase = "glyphicon glyphicon-arrow-down"
directionGlyph Decrease = "glyphicon glyphicon-arrow-up"

view :: InviteId -> State -> Html Action
view (InviteId inv) {users} = div []
    [ h2 [] [text "Edit the pick order and start the draft:"]
    , ol [] (mapIndex viewUser users)
    , form
        [ method "post"
        , encType "UrlEncoded"
        , action $ "/draft/invite/" <> inv <> "/launch"
        ] $
        (formUser <$> users)
        <> [button [className "btn btn-primary", type_ "submit"] [text "Start Draft"]]]

viewUser :: Int -> User -> Html Action
viewUser i {name} = li [] [dirButton Decrease, dirButton Increase, text name]
 where
    dirButton d =
        a
            [href "javascript:;", onClick \_ -> Move d i]
            [span [className $ directionGlyph d] []]

formUser :: forall a. User -> Html a
formUser {uid} = case uid of
    UserId n -> input [type_ "hidden", name "drafter", value (show n)] []

foreign import swap :: forall a. Array a -> Int -> Int -> Array a
foreign import mapIndex :: forall a b. (Int -> a -> b) -> Array a -> Array b
