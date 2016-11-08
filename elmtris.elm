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
boardWidth = 10
boardHeight = 22
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
          model |> updateGameState (\state -> state |> moveBrick (updatePosition toLeft) isBrickOnWallOrOtherBrick |> Maybe.withDefault state |> toGameplay)
        Right ->
          model |> updateGameState (\state -> state |> moveBrick (updatePosition toRight) isBrickOnWallOrOtherBrick |> Maybe.withDefault state |> toGameplay)
        Down ->
          model |> updateGameState (\state ->
            case state |> moveBrick (updatePosition down) doesBrickCollide of
              Just newState ->
                newState |> toGameplay  
              Nothing ->
                {state | board = state.board |> mergeElements state.brick |> removeLines} |> toGameplayWith (commandWithRandomBrickType NextBrick))
        Rotate ->
          model |> updateGameState (\state ->
            case state |> moveBrick updateRotation isBrickOnWallOrOtherBrick of
              Just newState ->
                newState |> toGameplay
              Nothing ->
                state |> moveBrick (updatePosition toLeft >> updateRotation) isBrickOnWallOrOtherBrick |> Maybe.withDefault state |> toGameplay)
        _ -> (model, Cmd.none)
    _ -> (model, Cmd.none)

rotate: Rotation -> Rotation
rotate rot =
  case rot of
    Horizontal Deg0 -> Vertical Deg90
    Vertical Deg90 -> Horizontal Deg180
    Horizontal Deg180 -> Vertical Deg270 
    Vertical Deg270 -> Horizontal Deg0

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

doesBrickCollide brick board =
  (isBrickOnGround brick board) || (isBrickOnOtherBrick brick board) || (isBrickOnWall brick board)

isBrickOnGround brick _ =
  brick.brickPos.y > boardHeight - (brickHeight brick)

isBrickOnWall brick _ =
  brick.brickPos.x < 0 || brick.brickPos.x > boardWidth - (brickWidth brick)

isBrickOnOtherBrick: Brick -> Board -> Bool
isBrickOnOtherBrick brick board =
  board
  |> Array2D.indexedMap (\row col cell ->
      brick
      |> brickShape
      |> Array2D.get (row - brick.brickPos.y) (col - brick.brickPos.x)
      |> Maybe.withDefault False
      |> (&&) cell)
  |> flattenArray2D
  |> Array.toList
  |> List.any identity

isBrickOnWallOrOtherBrick brick board =
  isBrickOnWall brick board || isBrickOnOtherBrick brick board

moveBrick: (Brick -> Brick) -> (Brick -> Board -> Bool) -> GameState -> Maybe GameState
moveBrick moveFunc collisionFunc state =
  let
    brick = state.brick
    newBrick = moveFunc brick 
  in
    if collisionFunc newBrick state.board then
      Nothing
    else
      Just {state | brick = newBrick}

down {brickPos} =
  {brickPos | y = brickPos.y + 1}

toLeft {brickPos} =
  {brickPos | x = brickPos.x - 1}

toRight brick =
  let
    pos = brick.brickPos
  in
    {pos | x = pos.x + 1}

updatePosition: (Brick -> Pos) -> Brick -> Brick
updatePosition changePosFunc brick =
    {brick | brickPos = changePosFunc brick}

updateRotation brick =
  {brick | rot = rotate brick.rot}

removeLines: Board -> Board
removeLines board =
  let
    remainingRows = board.data
      |> Array.filter (\row ->
        row
        |> Array.toList
        |> List.all identity
        |> not)
      |> Array.toList 
  in
    remainingRows
    |> List.append (List.repeat (boardHeight - (List.length remainingRows)) (Array.repeat boardWidth False))
    |> Array.fromList
    |> Array2D.fromArray

-- SUBSCRIPTIONS

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
    38 -> Move Rotate
    39 -> Move Right
    40 -> Move Down  
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
      List.concat [[ viewBorder ], mergeElements gameState.brick gameState.board |> viewBoard ]
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

mergeElements: Brick -> Board -> Board
mergeElements brick board  =
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