module Model exposing (..)

import Brick
import Board

type alias Score = Int

type alias GameState =
  { brick: Brick.Brick
  , linesCleared: Int
  , level: Int
  , score: Score
  , board: Board.Board
  , next: Bag
  }

type Model
  = Start
  | Gameplay GameState
  | Paused GameState
  | GameOver Score

type MoveType
  = Left
  | Right
  | Down
  | Rotate Brick.RotationDirection
  | None

type alias Bag = List Brick.BrickType 

type Msg
  = Begin
  | NextBag Bag
  | Tick
  | Move MoveType
  | Pause
  | DoNothing

init : ( Model, Cmd a )    
init = (Start, Cmd.none)