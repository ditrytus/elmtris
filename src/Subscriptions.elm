module Subscriptions exposing (subscriptions)

import Brick
import Model exposing (..)
import Keyboard exposing (..)
import Time exposing (Time, second)

subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    Start ->
      startNewGameOnKey 115
    Gameplay state ->
      Sub.batch
        [ Time.every (levelToDelay state.level) (\_ -> Move Down)
        , Keyboard.downs keyCodeToMove]
    GameOver _ -> 
      startNewGameOnKey 114

startNewGameOnKey : KeyCode -> Sub Msg
startNewGameOnKey keyToStart =
  Keyboard.presses (\keyCode -> if keyCode == keyToStart then Begin else DoNothing)

levelToDelay : Int -> Time.Time
levelToDelay level =
  Time.inMilliseconds <| 1000*0.9^(toFloat level-1)

keyCodeToMove: KeyCode -> Msg
keyCodeToMove keyCode =
  case keyCode of
    37 -> Move Left
    39 -> Move Right
    40 -> Move Down
    90 -> Move (Rotate Brick.CounterClockwise)
    88 -> Move (Rotate Brick.Clockwise)
    _ -> Move None