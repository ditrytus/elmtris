import Html exposing (Html)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Keyboard exposing (..)
import Random exposing (..)

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Array2DBool = List (List Bool)

make2DArray w h x =
  List.repeat h (List.repeat w x)

boardWidth = 9
boardHeight = 15
emptyBoard = make2DArray boardWidth boardHeight False

type alias Board = Array2DBool

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

type alias Pos = (Int, Int)

type alias Brick =
  { bType: BrickType
  , rot: Rotation
  }

type alias GameState =
  { brick: Brick
  , brickPos: Pos
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
  | Move MoveType
  | Reset

type alias BrickShape = Array2DBool

brickShape bType rot =
  case bType of
    Square ->
      [[True, True], [True, True]]
    Long ->
      case rot of
        Horizontal _ ->
          [[True, True, True, True]]
        Vertical _ ->
          [[True],[True],[True],[True]]
    ZLeft ->
      case rot of
        Vertical _ ->
          [[False, True], [True, True], [True, False]]
        Horizontal _ ->
          [[True, True, False], [False, True, True]]
    ZRight ->
      case rot of
        Vertical _ ->
          [[True, False], [True, True], [False, True]]
        Horizontal _ ->
          [[False, True, True], [True, True, False]]    
    LLeft ->
      case rot of
        Horizontal Deg0 ->
          [[True, True, True], [False, False, True]]
        Horizontal Deg180 ->
          [[True, False, False], [True, True, True]]
        Vertical Deg90 ->
          [[False, True], [False, True], [True, True]]
        Vertical Deg270 ->
          [[True, True], [True, False], [True, False]]
    LRight ->
      case rot of
        Horizontal Deg0 ->
          [[True, True, True], [True, False, False]]
        Horizontal Deg180 ->
          [[False, False, True], [True, True, True]]
        Vertical Deg90 ->
          [[True, True], [False, True], [False, True]]
        Vertical Deg270 ->
          [[True, False], [True, False], [True, True]]
    TLike ->
      case rot of
        Horizontal Deg0 ->
          [[False, True, False], [True, True, True]]
        Horizontal Deg180 ->
          [[True, True, True], [False, True, False]]
        Vertical Deg90 ->
          [[False, True], [True, True], [False, True]]
        Vertical Deg270 ->
          [[True, False], [True, True], [True, False]]
    
    
init = (Start, Cmd.none)

-- UPDATE

update msg model =
  case msg of
    Begin ->
      (model, generate FirstBrick (map intToBrickType (int 1 brickTypesCount)))
    FirstBrick newBrickType ->
      (Gameplay { brick = {bType=newBrickType, rot=Horizontal Deg0}, brickPos=(boardWidth // 2, 0), score = 0, board = emptyBoard}, Cmd.none)
    _ ->
      (Start, Cmd.none)

-- SUBSCRIPTIONS

subscriptions model =
  case model of
    Start ->
      Keyboard.presses (\_ -> Begin)
    _ ->
      Sub.none


-- VIEW

view model =
  svg [ viewBox "0 0 200 200", width "600", height "600" ]
    (viewContent model)

cellWidth = 10
cellHeight = 10

viewContent model =
  case model of
    Start ->
      [text' [x "50", y "50", textAnchor "middle", fontSize "5px"] [text "Press ANY key to start"]]
    Gameplay gameState ->
      List.concat [[ viewBorder ]]
    _ ->
      []

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
