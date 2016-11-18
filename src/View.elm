module View exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Model exposing (..)
import Brick
import Board exposing (Board)
import Update exposing (mergeWith)
import Array
import Array2D
import Array2DExtras exposing (flattenArray2D)
import Debug

type alias Pos = { x:Float, y:Float }
type alias Size = { width:Float, height:Float }

boardWidth : Int
boardWidth = cellWidth * Board.columns

boardHeight : Int
boardHeight = cellHeight * Board.visibleRows

displayWidth : Int
displayWidth = boardWidth * 2

displayHeight : Int
displayHeight = boardHeight

cellWidth : number
cellWidth = 10

cellHeight : number
cellHeight = 10

moveBy : Pos -> Pos -> Pos
moveBy a b = {x=a.x+b.x, y=a.y+b.y}

viewScale = (cellWidth, cellHeight)

nextBrickBoxPos : Pos
nextBrickBoxPos = Pos 11 10 |> scalePosToView

nextBrickBoxLogicSize = Size 6 4

nextBrickBoxSize : Size
nextBrickBoxSize = nextBrickBoxLogicSize |> scaleSizeToView

nextBrickLabelPos : Pos
nextBrickLabelPos = nextBrickBoxPos |> moveBy ((Pos 0.5 -0.1) |> scalePosToView)

scalePos : (Float,Float) -> Pos -> Pos
scalePos (h,v) pos = {x = pos.x * h, y = pos.y * v}

scalePosToView = scalePos viewScale

scaleSize : (Float,Float) -> Size -> Size
scaleSize (h,v) size = {width = size.width * h, height = size.height * v}

scaleSizeToView = scaleSize viewScale 

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
      List.concat
      [ [ viewBorder ]
      , gameState.board
        |> mergeWith gameState.brick
        |> Board.skipRows Board.obstructedRows
        |> viewBoard (Pos 0 0)
      , viewNextBrickBox gameState
      ]
    _ ->
      []

viewBorder : Svg a
viewBorder =
  rect
    [ x "0"
    , y "0"
    , width (toString boardWidth)
    , height (toString boardHeight)
    , fill "#FFFFFF"
    , stroke "#000000"
    , strokeWidth "1"
    ] []

viewNextBrickBox : GameState -> List (Svg a)
viewNextBrickBox state =
  List.concat
    [ [ rect
          [ x (toString nextBrickBoxPos.x)
          , y (toString nextBrickBoxPos.y)
          , width (toString nextBrickBoxSize.width)
          , height (toString nextBrickBoxSize.height)
          , fill "#FFFFFF"
          , stroke "#000000"
          , strokeWidth "1"
          ] []
      , text'
          [ x (toString nextBrickLabelPos.x)
          , y (toString nextBrickLabelPos.y)
          , textAnchor "start"
          , fontSize "10px"
          ]
          [text "Next brick"]
      ]
      , case state.next of
          brickType::_ ->
            Board.new (floor nextBrickBoxLogicSize.height) (floor nextBrickBoxLogicSize.width)
            |> mergeWith (Brick.Brick (Debug.log "next" brickType) Brick.Deg0 (Brick.Pos 1 1))
            |> Debug.log "board"
            |> viewBoard nextBrickBoxPos
          [] -> []
    ]
    
viewBoard: Pos -> Board -> List (Svg.Svg a) 
viewBoard pos board =
  let
    cellToRect: Int -> Int -> Bool -> Maybe (Svg.Svg a)
    cellToRect row column cell =
      case cell of
        True ->
          Just (
            rect
              [ x (toString ((floor pos.x) + column * cellWidth))
              , y (toString ((floor pos.y) + row * cellHeight))
              , width (toString (cellWidth + 0.1))
              , height (toString (cellHeight + 0.1))
              , strokeWidth "0"
              , fill "#000000" ]
              []
            )
        False ->
          Nothing
  in 
    board
    |> Array2D.indexedMap cellToRect
    |> flattenArray2D
    |> Array.toList
    |> List.filterMap identity