import Html.App as App
import Model exposing (init)
import Update exposing (update)
import Subscriptions exposing (subscriptions)
import View exposing (view)

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }