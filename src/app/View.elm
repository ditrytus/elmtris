module View exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Model exposing (..)
import Brick
import Board exposing (Board)
import Update exposing (mergeWith, ghostBrick)
import Array
import Array2D
import Array2DExtras exposing (flattenArray2D)

type alias Pos = { x:Float, y:Float }
type alias Size = { width:Float, height:Float }

type alias LabeledBox =
  { pos : Pos
  , size : Size
  , label : String
  }

boardSize : Size
boardSize =
  { width = cellSize.width * toFloat Board.columns 
  , height = cellSize.height * toFloat Board.visibleRows
  }

boardPos : Pos
boardPos = Pos 0 0

displaySize : Size
displaySize =
  { width = boardSize.width * 2
  , height = boardSize.height
  }

cellSize : Size
cellSize = Size 10 10

moveBy : Pos -> Pos -> Pos
moveBy a b = {x=a.x+b.x, y=a.y+b.y}

viewScale : ( Float, Float )
viewScale = (cellSize.width, cellSize.height)

nextBrickBox : LabeledBox
nextBrickBox =
  { pos = Pos 11 10
  , size = Size 6 4
  , label = "Next brick"
  }

scoreBox : LabeledBox
scoreBox =
  { pos = Pos 11 2
  , size = Size 8 2
  , label = "Score"
  }

levelBox : LabeledBox
levelBox =
  { pos = Pos 11 6
  , size = Size 3 2
  , label = "Level"
  }

linesBox : LabeledBox
linesBox =
  { pos = Pos 15 6
  , size = Size 4 2
  , label = "Lines"
  }

toView : LabeledBox -> LabeledBox
toView box =
  { pos = box.pos |> scalePosToView
  , size = box.size |> scaleSizeToView
  , label = box.label
  }

labelViewPos : LabeledBox -> Pos
labelViewPos box =
  box.pos |> moveBy (Pos 0.5 -0.1) |> scalePosToView

scalePos : (Float,Float) -> Pos -> Pos
scalePos (h,v) pos = {x = pos.x * h, y = pos.y * v}

scalePosToView : Pos -> Pos
scalePosToView = scalePos viewScale

scaleSize : (Float,Float) -> Size -> Size
scaleSize (h,v) size = {width = size.width * h, height = size.height * v}

scaleSizeToView : Size -> Size
scaleSizeToView = scaleSize viewScale 

black : String
black = "#000000"

gray : String
gray = "#DDDDDD"

instructions : List String
instructions =
  [ ""
  ,"arrows - move"
  ,"z, x - rotate"
  ,"space - drop"
  ,"p - pause"
  ,"g - ghost piece"
  ]

appendInstructions : List String -> List String
appendInstructions lines =
  List.concat [lines,instructions]

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
      ["Press S to start"] |> appendInstructions |> boardWithText
    Gameplay gameState ->
      List.concat
      [ ghostBrick gameState
        |> Board.skipRows Board.obstructedRows
        |> board gray boardPos
      , gameState.board
        |> mergeWith gameState.brick
        |> Board.skipRows Board.obstructedRows
        |> board black boardPos
      , [ boardBorder ]
      , showNextBrickBox gameState
      , showPointsBox gameState
      , showLevelBox gameState
      , showLinesBox gameState
      ]
    Paused gameState ->
      List.concat
      [ [ boardBorder ]
      , showLabeledBox nextBrickBox
      , showPointsBox gameState
      , showLevelBox gameState
      , showLinesBox gameState
      , ["Paused"] |> appendInstructions |> textInTheBoard
      ]
    GameOver overState ->
      List.concat
      [ [ boardBorder ]
      , showLabeledBox nextBrickBox
      , showPointsBox overState
      , showLevelBox overState
      , showLinesBox overState
      , ["Game Over", "Press R to restart"] |> appendInstructions |> textInTheBoard
      ]

boardWithText : List String -> List (Svg a)
boardWithText lines =
  List.concat
    [ [ boardBorder ]
    , showLabeledBox nextBrickBox 
    , showLabeledBox scoreBox
    , showLabeledBox levelBox
    , showLabeledBox linesBox
    , textInTheBoard lines
    ] 

textInTheBoard : List String -> List (Svg a)
textInTheBoard lines =
  let
    fSize = 10
    lineHeight = fSize + 2 
  in
    lines
    |> List.indexedMap (\i line -> 
    text'
      [ x <| toString <| (boardSize.width / 2)
      , y <| toString <| ((boardSize.height / 2) - (toFloat (List.length lines * lineHeight) / 2) + toFloat i * toFloat lineHeight)
      , textAnchor "middle"
      , Svg.Attributes.fontSize <| (toString fSize) ++ "px"
      ]
      [ text line ])

boardBorder : Svg a
boardBorder =
  rect
    [ x "0"
    , y "0"
    , width (toString boardSize.width)
    , height (toString boardSize.height)
    , fill "transparent"
    , stroke "#000000"
    , strokeWidth "1"
    ] []

showNextBrickBox : GameState -> List (Svg a)
showNextBrickBox state =
  List.concat
  [ showLabeledBox nextBrickBox
  , let
      viewBox = nextBrickBox |> toView
    in
      case state.next of
        brickType::_ ->
          Board.new (floor nextBrickBox.size.height) (floor nextBrickBox.size.width)
          |> mergeWith (Brick.Brick brickType Brick.Deg0 (Brick.Pos 1 1))
          |> board black viewBox.pos
        [] -> []
  ]

showPointsBox : {b | score:Int} -> List (Svg a)
showPointsBox state =
  showNumberBox scoreBox state.score

showLevelBox : {b | level:Int} -> List (Svg a)
showLevelBox state =
  showNumberBox levelBox state.level

showLinesBox : {b | linesCleared:Int} -> List (Svg a)
showLinesBox state =
  showNumberBox linesBox state.linesCleared

showNumberBox : LabeledBox -> Int -> List (Svg a)
showNumberBox box num =
  let
    numberPos =
      box.pos
      |> moveBy (Pos 0.5 ((box.size.height / 2) + 0.5))
      |> scalePosToView 
  in
    List.concat
    [ showLabeledBox box 
    , [ text'
        [ x (toString numberPos.x)
        , y (toString numberPos.y)
        , textAnchor "start"
        , fontSize "20px"
        ]
        [ text (num |> toString) ]
      ]
    ]

showLabeledBox : LabeledBox -> List (Svg a)
showLabeledBox box =
  let
    viewBox = box |> toView
    labelPos = labelViewPos box
  in
  [ rect
      [ x (toString viewBox.pos.x)
      , y (toString viewBox.pos.y)
      , width (toString viewBox.size.width)
      , height (toString viewBox.size.height)
      , fill "#FFFFFF"
      , stroke "#000000"
      , strokeWidth "1"
      ] []
  , text'
      [ x (toString labelPos.x)
      , y (toString labelPos.y)
      , textAnchor "start"
      , fontSize "10px"
      ]
      [text box.label]
  ]
    
board: String -> Pos -> Board -> List (Svg.Svg a) 
board fillColor pos board =
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
              , fill fillColor ]
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