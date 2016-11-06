import Html exposing (Html)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Keyboard exposing (..)
import Random exposing (..)
import Array2D
import Array
import Debug exposing (..)

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
boardWidth = 9
boardHeight = 15
emptyBoard = Array2D.repeat boardHeight boardWidth False

type alias Board = Array2D.Array2D Bool

type alias Score = Int

type BrickType
  = Square
  | Long
  | ZLeft
  | ZRight
  | LLeft
  | LRight
  | TLike

brickTypesCount = 7

intToBrickType i =
  case i of
    1 -> Square
    2 -> Long
    3 -> ZLeft
    4 -> ZRight
    5 -> LLeft
    6 -> LRight
    _ -> TLike

type Rotation
  = Vertical VerticalRotation
  | Horizontal HorizontalRotation

type VerticalRotation
  = Deg90
  | Deg270

type HorizontalRotation
  = Deg0
  | Deg180

type alias Pos = {x:Int, y:Int}

type alias Brick =
  { bType: BrickType
  , rot: Rotation
  , brickPos: Pos
  }

type alias GameState =
  { brick: Brick
  , score: Score
  , board: Board
  }

type Model
  = Start
  | Gameplay GameState
  | GameOver Score

type MoveType
  = Left
  | Right
  | Down
  | Rotate

type Msg
  = Begin
  | FirstBrick BrickType
  | NextBrick BrickType
  | Tick
  | Move MoveType
  | Reset

type alias BrickShape = Array2D.Array2D Bool

brickShape brick =
  case brick.bType of
    Square ->
      Array2D.fromList [[True, True], [True, True]]
    Long ->
      case brick.rot of
        Horizontal _ ->
          Array2D.fromList [[True, True, True, True]]
        Vertical _ ->
          Array2D.fromList [[True],[True],[True],[True]]
    ZLeft ->
      case brick.rot of
        Vertical _ ->
          Array2D.fromList [[False, True], [True, True], [True, False]]
        Horizontal _ ->
          Array2D.fromList [[True, True, False], [False, True, True]]
    ZRight ->
      case brick.rot of
        Vertical _ ->
          Array2D.fromList [[True, False], [True, True], [False, True]]
        Horizontal _ ->
          Array2D.fromList [[False, True, True], [True, True, False]]    
    LLeft ->
      case brick.rot of
        Horizontal Deg0 ->
          Array2D.fromList [[True, True, True], [False, False, True]]
        Horizontal Deg180 ->
          Array2D.fromList [[True, False, False], [True, True, True]]
        Vertical Deg90 ->
          Array2D.fromList [[False, True], [False, True], [True, True]]
        Vertical Deg270 ->
          Array2D.fromList [[True, True], [True, False], [True, False]]
    LRight ->
      case brick.rot of
        Horizontal Deg0 ->
          Array2D.fromList [[True, True, True], [True, False, False]]
        Horizontal Deg180 ->
          Array2D.fromList [[False, False, True], [True, True, True]]
        Vertical Deg90 ->
          Array2D.fromList [[True, True], [False, True], [False, True]]
        Vertical Deg270 ->
          Array2D.fromList [[True, False], [True, False], [True, True]]
    TLike ->
      case brick.rot of
        Horizontal Deg0 ->
          Array2D.fromList [[False, True, False], [True, True, True]]
        Horizontal Deg180 ->
          Array2D.fromList [[True, True, True], [False, True, False]]
        Vertical Deg90 ->
          Array2D.fromList [[False, True], [True, True], [False, True]]
        Vertical Deg270 ->
          Array2D.fromList [[True, False], [True, True], [True, False]]
    
    
init = (Start, Cmd.none)

-- UPDATE

update msg model =
  case msg of
    Begin ->
      (model, generate FirstBrick (map intToBrickType (int 1 brickTypesCount)))
    FirstBrick newBrickType ->
      (Gameplay { brick = {bType=newBrickType, rot=Horizontal Deg0, brickPos = Pos 0 0}, score = 0, board = emptyBoard}, Cmd.none)
    Tick ->
      case model of
        Gameplay state ->
          if doesBrickCollide state then
            (model, Cmd.none)
          else
            (Gameplay (moveBrickDown state), Cmd.none)
        _ -> (Start, Cmd.none) 
    _ -> (Start, Cmd.none)

brickHeight = brickShape >> Array2D.rows
brickWidth = brickShape >> Array2D.columns

doesBrickCollide state =
  state.brick.brickPos.y == boardHeight - (brickHeight state.brick)

moveBrickDown state =
  let
    brick = state.brick
    brickPos = brick.brickPos
    newPos = {brickPos | y = brickPos.y + 1}
    newBrick = {brick | brickPos = newPos}
  in
    {state | brick = newBrick}

-- SUBSCRIPTIONS

subscriptions model =
  case model of
    Start ->
      Keyboard.presses (\_ -> Begin)
    Gameplay _ ->
      Time.every Time.second (\_ -> Tick)
    _ -> 
      Sub.none

-- VIEW
view : Model -> Html a
view model =
  svg [ viewBox "0 0 200 200", width "600", height "600" ]
    (viewContent model)

cellWidth : number
cellWidth = 10

cellHeight : number
cellHeight = 10

viewContent : Model -> List (Svg a)
viewContent model =
  case model of
    Start ->
      [text' [x "50", y "50", textAnchor "middle", fontSize "5px"] [text "Press ANY key to start"]]
    Gameplay gameState ->
      List.concat [[ viewBorder ], mergeElements gameState.board (gameState.brick |> Debug.log "brick") |> viewBoard ]
    _ ->
      []

viewBorder : Svg a
viewBorder =
  rect
    [ x "0"
    , y "0"
    , width (toString (boardWidth * cellWidth))
    , height (toString (boardHeight * cellHeight))
    , fill "#FFFFFF"
    , stroke "#000000"
    , strokeWidth "1"
    ] []

or a b = a || b

mergeElements: Board -> Brick -> Board
mergeElements board brick =
    board |>
      Array2D.indexedMap (\row column cell ->
        brickShape brick
        |> Array2D.get (row - brick.brickPos.y) (column - brick.brickPos.x)
        |> Maybe.withDefault False
        |> or cell) 

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
            , width (toString cellWidth)
            , height (toString cellHeight)
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

hasValue: Maybe a -> Bool
hasValue maybe =
  case maybe of
    Just a -> True
    Nothing -> False

flattenArray2D: Array2D.Array2D a -> Array.Array a
flattenArray2D array2D =
  array2D.data
  |> Array.map (\row -> Array.toList row)
  |> Array.toList
  |> List.concat
  |> Array.fromList