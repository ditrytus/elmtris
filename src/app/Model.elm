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
  , ghostBrickEnabled: Bool
  }

type alias GameOverState =
  { linesCleared: Int
  , level: Int
  , score: Score
  }

type Model
  = Start
  | Gameplay GameState
  | Paused GameState
  | GameOver GameOverState

type MoveType
  = Left
  | Right
  | Down
  | Rotate Brick.RotationDirection
  | None
  | Drop

type alias Bag = List Brick.BrickType 

type Msg
  = Begin
  | NextBag Bag
  | Tick
  | Move MoveType
  | Pause
  | ToggleGhostBrick
  | DoNothing

init : ( Model, Cmd a )    
init = (Start, Cmd.none)