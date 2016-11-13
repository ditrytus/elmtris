module View exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Model exposing (..)
import Board exposing (Board)
import Update exposing (mergeElements)
import Array
import Array2D
import Array2DExtras exposing (flattenArray2D)

displayWidth : Int
displayWidth = cellWidth * Board.width

displayHeight : Int
displayHeight = cellHeight * Board.height

cellWidth : number
cellWidth = 10

cellHeight : number
cellHeight = 10

-- VIEW
view : Model -> Html a
view model =
  svg
  [ viewBox (
      "0 0 "
      ++ toString displayWidth
      ++ " "
      ++ toString displayHeight)
  , width "100%"
  , height "100%"
  ]
  (viewContent model)

viewContent : Model -> List (Svg a)
viewContent model =
  case model of
    Start ->
      [text' [x "50", y "50", textAnchor "middle", fontSize "5px"] [text "Press ANY key to start"]]
    Gameplay gameState ->
      List.concat [[ viewBorder ], mergeElements gameState.brick gameState.board |> viewBoard ]
    _ ->
      []

viewBorder : Svg a
viewBorder =
  rect
    [ x "0"
    , y "0"
    , width (toString (Board.width * cellWidth))
    , height (toString (Board.height * cellHeight))
    , fill "#FFFFFF"
    , stroke "#000000"
    , strokeWidth "1"
    ] []

viewBoard: Board -> List (Svg.Svg a) 
viewBoard board =
  let
    cellToRect: Int -> Int -> Bool -> Maybe (Svg.Svg a)
    cellToRect row column cell =
      case cell of
        True ->
          Just (rect
            [ x (toString (column * cellWidth))
            , y (toString (row * cellHeight))
            , width (toString (cellWidth + 0.1))
            , height (toString (cellHeight + 0.1))
            , strokeWidth "0"
            , fill "#000000" ]
            [])
        False ->
          Nothing
  in 
    board
    |> Array2D.indexedMap cellToRect
    |> flattenArray2D
    |> Array.toList
    |> List.filterMap identity