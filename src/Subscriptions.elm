module Subscriptions exposing (subscriptions)

import Brick
import Model exposing (..)
import Keyboard exposing (..)
import Time exposing (Time, second)

subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    Start ->
      Keyboard.presses (\_ -> Begin)
    Gameplay _ ->
      Sub.batch
        [ Time.every Time.second (\_ -> Move Down)
        , Keyboard.downs keyCodeToMove]
    _ -> 
      Sub.none

keyCodeToMove: KeyCode -> Msg
keyCodeToMove keyCode =
  case keyCode of
    37 -> Move Left
    39 -> Move Right
    40 -> Move Down
    90 -> Move (Rotate Brick.CounterClockwise)
    88 -> Move (Rotate Brick.Clockwise)
    _ -> Move None