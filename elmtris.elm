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
  | None

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
      (model, commandWithRandomBrickType FirstBrick)
    FirstBrick newBrickType ->
      { brick = newBrick newBrickType, score = 0, board = emptyBoard} |> toGameplay
    NextBrick newBrickType ->
      model |> updateGameState (\state -> {state | brick = newBrick newBrickType} |> toGameplay)
    Move moveType ->
      case moveType of
        Left ->
          model |> updateGameState (\state -> state |> moveBrick toLeft |> toGameplay)
        Right ->
          model |> updateGameState (\state -> state |> moveBrick toRight |> toGameplay)
        _ -> (model, Cmd.none)
    Tick ->
      model |> updateGameState (\state -> 
        if doesBrickCollide state then
          {state | board = mergeElements state.board state.brick} |> toGameplayWith (commandWithRandomBrickType NextBrick)
        else
          state |> moveBrick down |> toGameplay)
    _ -> (model, Cmd.none)

toGameplayWith cmd state = (Gameplay state, cmd)

toGameplay = toGameplayWith Cmd.none

updateGameState: (GameState -> (Model, Cmd a)) -> Model -> (Model, Cmd a)
updateGameState updateStateFunc model =
  case model of
      Gameplay state ->
        updateStateFunc state
      _ -> (model, Cmd.none)

newBrick brickType =
  {bType=brickType, rot=Horizontal Deg0, brickPos = Pos 0 0}

commandWithRandomBrickType cmd =
  generate cmd (map intToBrickType (int 1 brickTypesCount))

brickHeight = brickShape >> Array2D.rows
brickWidth = brickShape >> Array2D.columns

doesBrickCollide state =
  (isBrickOnGround state) || (isTouchingOtherBrick state) 

isBrickOnGround state =
  state.brick.brickPos.y == boardHeight - (brickHeight state.brick)

isTouchingOtherBrick state =
  state.board
  |> Array2D.getRow (state.brick.brickPos.y + (brickHeight state.brick))
  |> Maybe.withDefault Array.empty
  |> Array.slice state.brick.brickPos.x (state.brick.brickPos.x + (brickWidth state.brick))
  |> Array.toList
  |> List.indexedMap (\i cell ->
      brickShape state.brick
      |> Array2D.get i ((brickHeight state.brick)-1)
      |> Maybe.withDefault False
      |> (&&) cell)
  |> List.any identity

moveBrick: (GameState -> Pos -> Pos) -> GameState -> GameState
moveBrick moveFunc state =
  let
    brick = state.brick
    brickPos = brick.brickPos
    newPos = moveFunc state brickPos
    newBrick = {brick | brickPos = newPos}
  in
    {state | brick = newBrick}

down _ pos = {pos | y = pos.y + 1}

toLeft _ pos = {pos | x = Basics.max (pos.x - 1) 0}

toRight state pos = {pos | x = Basics.min (pos.x + 1) (boardWidth - (brickWidth state.brick))}

-- SUBSCRIPTIONS

subscriptions model =
  case model of
    Start ->
      Keyboard.presses (\_ -> Begin)
    Gameplay _ ->
      Sub.batch
        [ Time.every Time.second (\_ -> Tick)
        , Keyboard.downs keyCodeToMove]
    _ -> 
      Sub.none

keyCodeToMove: KeyCode -> Msg
keyCodeToMove keyCode =
  case keyCode of
    37 -> Move Left
    38 -> Move None --Move Rotate
    39 -> Move Right
    40 -> Move None --Move Down  
    _ -> Move None

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
      List.concat [[ viewBorder ], mergeElements gameState.board gameState.brick |> viewBoard ]
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

mergeElements: Board -> Brick -> Board
mergeElements board brick =
    board |>
      Array2D.indexedMap (\row column cell ->
        brickShape brick
        |> Array2D.get (row - brick.brickPos.y) (column - brick.brickPos.x)
        |> Maybe.withDefault False
        |> (||) cell) 

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