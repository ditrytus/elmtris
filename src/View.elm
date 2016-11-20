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

type alias Pos = { x:Float, y:Float }
type alias Size = { width:Float, height:Float }

boardSize : Size
boardSize =
  { width = cellSize.width * toFloat Board.columns 
  , height = cellSize.height * toFloat Board.visibleRows
  }

displaySize : Size
displaySize =
  { width = boardSize.width * 2
  , height = boardSize.height
  }

cellSize : Size
cellSize = Size 10 10

moveBy : Pos -> Pos -> Pos
moveBy a b = {x=a.x+b.x, y=a.y+b.y}

viewScale = (cellSize.width, cellSize.height)

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
      ++ toString displaySize.width
      ++ " "
      ++ toString displaySize.height)
  , width "100%"
  , height "100%"
  ]
  (content model)

content : Model -> List (Svg a)
content model =
  case model of
    Start ->
      boardWithText "Press S to start"
    Gameplay gameState ->
      List.concat
      [ [ boardBorder ]
      , gameState.board
        |> mergeWith gameState.brick
        |> Board.skipRows Board.obstructedRows
        |> board (Pos 0 0)
      , nextBrickBox gameState
      ]
    GameOver score ->
      boardWithText "Game Over"

boardWithText txt =
  List.concat
    [ [ boardBorder ]
    , nextBrickLabeledBorder
    , [ text'
        [ x <| toString <| boardSize.width / 2
        , y <| toString <| boardSize.height / 2
        , textAnchor "middle"
        , fontSize "10px"
        ]
        [ text txt ]
      ] 
    ]

boardBorder : Svg a
boardBorder =
  rect
    [ x "0"
    , y "0"
    , width (toString boardSize.width)
    , height (toString boardSize.height)
    , fill "#FFFFFF"
    , stroke "#000000"
    , strokeWidth "1"
    ] []

nextBrickBox : GameState -> List (Svg a)
nextBrickBox state =
  List.concat
    [ nextBrickLabeledBorder
      , case state.next of
          brickType::_ ->
            Board.new (floor nextBrickBoxLogicSize.height) (floor nextBrickBoxLogicSize.width)
            |> mergeWith (Brick.Brick (Debug.log "next" brickType) Brick.Deg0 (Brick.Pos 1 1))
            |> Debug.log "board"
            |> board nextBrickBoxPos
          [] -> []
    ]

nextBrickLabeledBorder : List (Svg a)
nextBrickLabeledBorder =
  [ rect
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
    
board: Pos -> Board -> List (Svg.Svg a) 
board pos board =
  let
    cellToRect: Int -> Int -> Bool -> Maybe (Svg.Svg a)
    cellToRect row column cell =
      case cell of
        True ->
          Just (
            rect
              [ x (toString (floor pos.x + column * floor cellSize.width))
              , y (toString (floor pos.y + row * floor cellSize.height))
              , width (toString (cellSize.width + 0.1))
              , height (toString (cellSize.height + 0.1))
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