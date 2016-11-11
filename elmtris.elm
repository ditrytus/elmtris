import Html exposing (Html)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Keyboard exposing (..)
import Random exposing (..)
import Array2D
import Array

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
boardWidth : Int
boardWidth = 10

boardHeight : Int
boardHeight = 20


type alias Board = Array2D.Array2D Bool

emptyBoard : Board
emptyBoard = Array2D.repeat boardHeight boardWidth False

type alias Score = Int

type BrickType
  = Square
  | Long
  | ZLeft
  | ZRight
  | LLeft
  | LRight
  | TLike

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

brickShape : Brick -> BrickShape
brickShape {bType, rot} =
  case bType of
    Square ->
      Array2D.fromList [[True, True], [True, True]]
    Long ->
      case rot of
        Horizontal _ ->
          Array2D.fromList [[True, True, True, True]]
        Vertical _ ->
          Array2D.fromList [[True],[True],[True],[True]]
    ZLeft ->
      case rot of
        Vertical _ ->
          Array2D.fromList [[False, True], [True, True], [True, False]]
        Horizontal _ ->
          Array2D.fromList [[True, True, False], [False, True, True]]
    ZRight ->
      case rot of
        Vertical _ ->
          Array2D.fromList [[True, False], [True, True], [False, True]]
        Horizontal _ ->
          Array2D.fromList [[False, True, True], [True, True, False]]    
    LLeft ->
      case rot of
        Horizontal Deg0 ->
          Array2D.fromList [[True, True, True], [False, False, True]]
        Horizontal Deg180 ->
          Array2D.fromList [[True, False, False], [True, True, True]]
        Vertical Deg90 ->
          Array2D.fromList [[True, True], [True, False], [True, False]]
        Vertical Deg270 ->
          Array2D.fromList [[False, True], [False, True], [True, True]]
    LRight ->
      case rot of
        Horizontal Deg0 ->
          Array2D.fromList [[True, True, True], [True, False, False]]
        Horizontal Deg180 ->
          Array2D.fromList [[False, False, True], [True, True, True]]
        Vertical Deg90 ->
          Array2D.fromList [[True, False], [True, False], [True, True]]
        Vertical Deg270 ->
          Array2D.fromList [[True, True], [False, True], [False, True]]
    TLike ->
      case rot of
        Horizontal Deg0 ->
          Array2D.fromList [[False, True, False], [True, True, True]]
        Horizontal Deg180 ->
          Array2D.fromList [[True, True, True], [False, True, False]]
        Vertical Deg90 ->
          Array2D.fromList [[False, True], [True, True], [False, True]]
        Vertical Deg270 ->
          Array2D.fromList [[True, False], [True, True], [True, False]]

init : ( Model, Cmd a )    
init = (Start, Cmd.none)

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
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

rotate : Rotation -> Rotation
rotate rot =
  case rot of
    Horizontal Deg0 -> Vertical Deg270
    Vertical Deg90 -> Horizontal Deg0
    Horizontal Deg180 -> Vertical Deg90 
    Vertical Deg270 -> Horizontal Deg180

toGameplayWith : a -> GameState -> ( Model, a )
toGameplayWith cmd state = (Gameplay state, cmd)

toGameplay : GameState -> ( Model, Cmd a )
toGameplay = toGameplayWith Cmd.none

updateGameState : (GameState -> (Model, Cmd a)) -> Model -> (Model, Cmd a)
updateGameState updateStateFunc model =
  case model of
      Gameplay state ->
        updateStateFunc state
      _ -> (model, Cmd.none)

newBrick : BrickType -> Brick
newBrick brickType =
  let
    angle = Horizontal Deg0
    shape = {bType = brickType, rot = angle, brickPos = Pos 0 0}
  in
    {bType=brickType, rot=angle, brickPos = Pos ((boardWidth - (brickWidth shape)) // 2) (brickHeight shape)}

commandWithRandomBrickType : (BrickType -> a) -> Cmd a
commandWithRandomBrickType cmd =
  let
    brickTypesCount = 7
  in
    generate cmd (map intToBrickType (int 1 brickTypesCount))

brickHeight : Brick -> Int
brickHeight = brickShape >> Array2D.rows

brickWidth : Brick -> Int
brickWidth = brickShape >> Array2D.columns

brickOriginX : Brick -> Int
brickOriginX brick = brick.brickPos.x

brickOriginY : Brick -> Int
brickOriginY brick = brick.brickPos.y - brickHeight brick 

doesBrickCollide : Brick -> Board -> Bool
doesBrickCollide brick board =
  (isBrickOnGround brick board) || (isBrickOnOtherBrick brick board) || (isBrickOnWall brick board)

isBrickOnGround : Brick -> Board -> Bool
isBrickOnGround brick _ =
  brick.brickPos.y > boardHeight

isBrickOnWall : Brick -> Board -> Bool
isBrickOnWall brick _ =
  brick.brickPos.x < 0 || brick.brickPos.x > boardWidth - (brickWidth brick)

isBrickOnOtherBrick : Brick -> Board -> Bool
isBrickOnOtherBrick brick board =
  board
  |> Array2D.indexedMap (\row col cell ->
      brick
      |> isBrickAt row col
      |> (&&) cell)
  |> flattenArray2D
  |> Array.toList
  |> List.any identity

isBrickAt : Int -> Int -> Brick -> Bool
isBrickAt row col brick =
  brick
  |> brickShape 
  |> Array2D.get (row - brickOriginY brick) (col - brickOriginX brick)
  |> Maybe.withDefault False

isBrickOnWallOrOtherBrick : Brick -> Board -> Bool
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

down : Brick -> Pos
down {brickPos} =
  {brickPos | y = brickPos.y + 1}

toLeft : Brick -> Pos
toLeft {brickPos} =
  {brickPos | x = brickPos.x - 1}

toRight : Brick -> Pos
toRight brick =
  let
    pos = brick.brickPos
  in
    {pos | x = pos.x + 1}

updatePosition: (Brick -> Pos) -> Brick -> Brick
updatePosition changePosFunc brick =
    {brick | brickPos = changePosFunc brick}

updateRotation: Brick -> Brick
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
    38 -> Move Rotate
    39 -> Move Right
    40 -> Move Down  
    _ -> Move None

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

displayWidth : Int
displayWidth = cellWidth * boardWidth

displayHeight : Int
displayHeight = cellHeight * boardHeight

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
      Array2D.indexedMap (\row col cell ->
        brick
        |> isBrickAt row col
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