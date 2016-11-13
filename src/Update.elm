module Update exposing (..)

import Model exposing (..)
import Brick exposing (Brick, BrickType, intToBrickType, Pos)
import Board exposing (Board)
import Random exposing (..)
import Array
import Array2D
import Array2DExtras exposing (flattenArray2D)

update : Msg -> Model.Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Begin ->
      (model, commandWithRandomBrickType FirstBrick)
    FirstBrick newBrickType ->
      { brick = Brick.new Board.width newBrickType, score = 0, board = Board.empty} |> toGameplay
    NextBrick newBrickType ->
      model |> updateGameState (\state -> {state | brick = Brick.new Board.width newBrickType} |> toGameplay)
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
                {state | board = state.board |> mergeElements state.brick |> Board.removeLines} |> toGameplayWith (commandWithRandomBrickType NextBrick))
        Rotate ->
          model |> updateGameState (\state ->
            case state |> moveBrick updateRotation isBrickOnWallOrOtherBrick of
              Just newState ->
                newState |> toGameplay
              Nothing ->
                state |> moveBrick (updatePosition toLeft >> updateRotation) isBrickOnWallOrOtherBrick |> Maybe.withDefault state |> toGameplay)
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

doesBrickCollide : Brick -> Board -> Bool
doesBrickCollide brick board =
  (isBrickOnGround brick board) || (isBrickOnOtherBrick brick board) || (isBrickOnWall brick board)

isBrickOnGround : Brick -> Board -> Bool
isBrickOnGround brick _ =
  brick.brickPos.y > Board.height

isBrickOnWall : Brick -> Board -> Bool
isBrickOnWall brick _ =
  brick.brickPos.x < 0 || brick.brickPos.x > Board.width - (Brick.width brick)

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
  |> Brick.shape 
  |> Array2D.get (row - Brick.originY brick) (col - Brick.originX brick)
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
  {brick | rot = Brick.rotate brick.rot}

mergeElements: Brick -> Board -> Board
mergeElements brick board  =
    board |>
      Array2D.indexedMap (\row col cell ->
        brick
        |> isBrickAt row col
        |> (||) cell)