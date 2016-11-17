module Update exposing (..)

import Model exposing (..)
import Brick exposing (Brick, BrickType, intToBrickBag, Pos, isAt, RotationDirection)
import Board exposing (..)
import Random exposing (..)
import Array
import Array2D
import Array2DExtras exposing (flattenArray2D)
import List.Extra

visibleNextBricks : Int
visibleNextBricks = 1 

update : Msg -> Model.Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Begin ->
      Gameplay { brick = Brick.new Board.columns Brick.I, score = 0, board = Board.empty, next = [] }
      |> updateGameState nextBrick
    NextBag newBag->
      model
      |> updateGameState (\state -> Gameplay {state | next = List.concat [state.next, newBag] }
      |> updateGameState nextBrick)
    Move moveType ->
      case moveType of
        Left ->
          model |> updateGameState (\state -> state |> moveBrick (updatePosition toLeft) |> Maybe.withDefault state |> toGameplay)
        Right ->
          model |> updateGameState (\state -> state |> moveBrick (updatePosition toRight) |> Maybe.withDefault state |> toGameplay)
        Down ->
          model |> updateGameState (\state ->
            case state |> moveBrick (updatePosition down) of
              Just newState ->
                newState |> toGameplay  
              Nothing ->
                Gameplay {state | board = state.board |> mergeElements state.brick |> Board.removeLines}
                |> updateGameState nextBrick)
        Rotate direction ->
          model |> updateGameState (\state -> state |> moveBrick (updateRotation direction state) |> Maybe.withDefault state |> toGameplay)
        _ -> (model, Cmd.none)
    _ -> (model, Cmd.none)
    
nextBrick : GameState -> ( Model, Cmd Msg )
nextBrick state =
  let
    nextRandomBag = commandWithRandomBrickBag NextBag
  in
    if (List.length state.next) + 1 >= visibleNextBricks then
      case state.next of
        t::rest ->
          {state | brick = Brick.new Board.columns t, next = rest}  
          |> toGameplay
        [] ->
          state |> toGameplayWith nextRandomBag
    else
      state |> toGameplayWith nextRandomBag

toGameplayWith : Cmd a -> GameState -> ( Model, Cmd a )
toGameplayWith cmd state = (Gameplay state, cmd)

toGameplay : GameState -> ( Model, Cmd a )
toGameplay = toGameplayWith Cmd.none

updateGameState : (GameState -> (Model, Cmd a)) -> Model -> (Model, Cmd a)
updateGameState updateStateFunc model =
  case model of
      Gameplay state ->
        updateStateFunc state
      _ -> (model, Cmd.none)

commandWithRandomBrickBag : (List BrickType -> a) -> Cmd a
commandWithRandomBrickBag cmd =
  let
    bagsCount = List.Extra.permutations Brick.allBrickTypes |> List.length
  in
    generate cmd (map intToBrickBag (int 1 bagsCount))

moveBrick: (Brick -> Brick) -> GameState -> Maybe GameState
moveBrick moveFunc state =
  let
    brick = state.brick
    newBrick = moveFunc brick 
  in
    if doesCollide newBrick state.board then
      Nothing
    else
      Just {state | brick = newBrick}

doesCollide : Brick -> Board -> Bool
doesCollide brick board =
  brick
  |> Brick.shape
  |> Array2D.indexedMap (\row column cell ->
      board
      |> Array2D.get (row + brick.brickPos.y) (column + brick.brickPos.x)
      |> Maybe.withDefault True
      |> (&&) cell)
  |> flattenArray2D
  |> Array.toList
  |> List.any identity

down : Brick -> Pos
down = by {x=0, y=1}

toLeft : Brick -> Pos
toLeft = by {x=-1, y=0}

toRight : Brick -> Pos
toRight = by {x=1, y=0}

by : Pos -> Brick -> Pos
by t brick =
  let
    pos = brick.brickPos
  in
    {x = pos.x + t.x, y = pos.y + t.y}

updatePosition: (Brick -> Pos) -> Brick -> Brick
updatePosition changePosFunc brick =
    {brick | brickPos = changePosFunc brick}

updateRotation: RotationDirection -> GameState -> Brick -> Brick
updateRotation direction state brick =
  let
    rotatedBrick = {brick | rot = Brick.rotate direction brick.rot}
    kicks = Brick.wallKicks brick.rot direction brick.bType
  in
    kicks
    |> List.filterMap (\kick -> 
      moveBrick (updatePosition (by kick)) {state | brick = rotatedBrick})
    |> List.head
    |> Maybe.withDefault state
    |> (.brick)


mergeElements: Brick -> Board -> Board
mergeElements brick board  =
    board |>
      Array2D.indexedMap (\row col cell ->
        brick
        |> Brick.isAt (row - brick.brickPos.y) (col - brick.brickPos.x)
        |> (||) cell)

