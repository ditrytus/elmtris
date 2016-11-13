module Model exposing (..)

import Brick
import Board

type alias Score = Int

type alias GameState =
  { brick: Brick.Brick
  , score: Score
  , board: Board.Board
  }

type Model
  = Start
  | Gameplay GameState
  | GameOver Score

type MoveType
  = Left
  | Right
  | Down
  | Rotate Brick.RotationDirection
  | None

type Msg
  = Begin
  | FirstBrick Brick.BrickType
  | NextBrick Brick.BrickType
  | Tick
  | Move MoveType
  | Reset

init : ( Model, Cmd a )    
init = (Start, Cmd.none)