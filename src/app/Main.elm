import Html
import Model exposing (init)
import Update exposing (update)
import Subscriptions exposing (subscriptions)
import View exposing (view)

main : Program Never Model.Model Model.Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }