module Update exposing (..)

import Model exposing (..)
import Brick exposing (Brick, BrickType, intToBrickBag, Pos, isAt, RotationDirection)
import Board exposing (..)
import Random exposing (..)
import Array
import Array2D
import Array2DExtras exposing (flattenArray2D)
import List.Extra

type alias BrickUpdateFunc = (Brick -> Brick)
type alias MoveFunc = (Brick -> Pos)

visibleNextBricks : Int
visibleNextBricks = 1 

update : Msg -> Model.Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Begin ->
      Gameplay { brick = Brick.new Board.columns Brick.I, score = 0, board = Board.empty, next = [] }
      |> updateGameState byTakingNextBrick
    NextBag newBag->
      model |> updateGameState (bySetingNewBagAndTakingNextBrick newBag)
    Move moveType ->
      case moveType of
        Left ->
          model |> updateGameState (byMovingBrick toLeft)
        Right ->
          model |> updateGameState (byMovingBrick toRight)
        Down ->
          model |> updateGameState (\state ->
            case state |> updateBrickWithCollision (updatePosition down) of
              Just newState ->
                newState |> toGameplay  
              Nothing ->
                if state.brick.brickPos.y >= Board.obstructedRows then
                  Gameplay {state | board = state.board |> mergeWith state.brick |> Board.removeLines}
                  |> updateGameState byTakingNextBrick
                else
                  (GameOver state.score, Cmd.none))                  
        Rotate direction ->
          model |> updateGameState (byRotatingBrickIn direction)
        _ -> (model, Cmd.none)
    _ -> (model, Cmd.none)

bySetingNewBagAndTakingNextBrick : Model.Bag -> GameState ->  ( Model, Cmd Msg )
bySetingNewBagAndTakingNextBrick newBag state =
  {state | next = List.concat [state.next, newBag] }
  |> byTakingNextBrick

byMovingBrick : MoveFunc -> GameState ->  ( Model, Cmd a )
byMovingBrick translateFunc state  =
  byUpdatingBrick (updatePosition translateFunc) state

byRotatingBrickIn : RotationDirection -> GameState -> ( Model, Cmd a )
byRotatingBrickIn direction state =
  byUpdatingBrick (updateRotation direction state) state

byUpdatingBrick : BrickUpdateFunc -> GameState -> ( Model, Cmd a )
byUpdatingBrick brickUpdateFunc state  =
  state
  |> updateBrickWithCollision brickUpdateFunc
  |> Maybe.withDefault state
  |> toGameplay

byTakingNextBrick : GameState -> ( Model, Cmd Msg )
byTakingNextBrick state =
  let
    nextRandomBag = commandWithRandomBrickBag NextBag
  in
    if (List.length state.next) > visibleNextBricks then
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

updateBrickWithCollision: BrickUpdateFunc -> GameState -> Maybe GameState
updateBrickWithCollision brickUpdateFunc state =
  let
    brick = state.brick
    newBrick = brickUpdateFunc brick 
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

down : MoveFunc
down = by {x=0, y=1}

toLeft : MoveFunc
toLeft = by {x=-1, y=0}

toRight : MoveFunc
toRight = by {x=1, y=0}

by : Pos -> MoveFunc
by t brick =
  let
    pos = brick.brickPos
  in
    {x = pos.x + t.x, y = pos.y + t.y}

updatePosition: MoveFunc -> BrickUpdateFunc
updatePosition changePosFunc brick =
    {brick | brickPos = changePosFunc brick}

updateRotation: RotationDirection -> GameState -> BrickUpdateFunc
updateRotation direction state brick =
  let
    rotatedBrick = {brick | rot = Brick.rotate direction brick.rot}
    kicks = Brick.wallKicks brick.rot direction brick.bType
  in
    kicks
    |> List.filterMap (\kick -> 
      updateBrickWithCollision (updatePosition (by kick)) {state | brick = rotatedBrick})
    |> List.head
    |> Maybe.withDefault state
    |> (.brick)


mergeWith: Brick -> Board -> Board
mergeWith brick board  =
    board |>
      Array2D.indexedMap (\row col cell ->
        brick
        |> Brick.isAt (row - brick.brickPos.y) (col - brick.brickPos.x)
        |> (||) cell)

