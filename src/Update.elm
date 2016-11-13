module Update exposing (..)

import Model exposing (..)
import Brick exposing (Brick, BrickType, intToBrickType, Pos, isAt)
import Board exposing (..)
import Random exposing (..)
import Array
import Array2D
import Array2DExtras exposing (flattenArray2D)
import Debug

update : Msg -> Model.Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Begin ->
      (model, commandWithRandomBrickType FirstBrick)
    FirstBrick newBrickType ->
      { brick = Brick.new Board.columns newBrickType, score = 0, board = Board.empty} |> toGameplay
    NextBrick newBrickType ->
      model |> updateGameState (\state -> {state | brick = Brick.new Board.columns newBrickType} |> toGameplay)
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
                {state | board = state.board |> mergeElements state.brick |> Board.removeLines} |> toGameplayWith (commandWithRandomBrickType NextBrick))
        Rotate ->
          model |> updateGameState (\state ->
            case state |> moveBrick updateRotation of
              Just newState ->
                newState |> toGameplay
              Nothing ->
                state |> moveBrick (updatePosition toLeft >> updateRotation) |> Maybe.withDefault state |> toGameplay)
        _ -> (model, Cmd.none)
    _ -> (model, Cmd.none)

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

commandWithRandomBrickType : (BrickType -> a) -> Cmd a
commandWithRandomBrickType cmd =
  let
    brickTypesCount = 7
  in
    generate cmd (map intToBrickType (int 1 brickTypesCount))

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
  {brick | rot = Brick.rotate brick.rot}

mergeElements: Brick -> Board -> Board
mergeElements brick board  =
    board |>
      Array2D.indexedMap (\row col cell ->
        brick
        |> Brick.isAt (row - brick.brickPos.y) (col - brick.brickPos.x)
        |> (||) cell)

