module Brick exposing (..)

import Array2D
import List.Extra

type BrickType
  = O | I | Z | S | J | L | T

allBrickTypes : List BrickType
allBrickTypes = [O, I, Z, S, J, L, T]

intToBrickBag : Int -> List BrickType
intToBrickBag i =
  List.Extra.permutations allBrickTypes
  |> List.Extra.getAt i
  |> Maybe.withDefault allBrickTypes

type Rotation
  = Deg0
  | Deg90
  | Deg180
  | Deg270

type RotationDirection
  = Clockwise
  | CounterClockwise

rotate : RotationDirection -> Rotation -> Rotation
rotate direction rot =
  case direction of
    Clockwise ->
      case rot of
        Deg0 -> Deg270
        Deg90 -> Deg0
        Deg180 -> Deg90 
        Deg270 -> Deg180
    CounterClockwise ->
      case rot of
        Deg0 -> Deg90
        Deg90 -> Deg180
        Deg180 -> Deg270 
        Deg270 -> Deg0

type alias Pos = {x:Int, y:Int}

type alias Brick =
  { bType: BrickType
  , rot: Rotation
  , brickPos: Pos
  }

isAt : Int -> Int -> Brick -> Bool
isAt row col brick =
  brick
  |> shape
  |> Array2D.get row col
  |> Maybe.withDefault False

type alias BrickShape = Array2D.Array2D Bool

shape : Brick -> BrickShape
shape {bType, rot} =
  case bType of
    O ->
      Array2D.fromList [
        [0, 1, 1, 0],
        [0, 1, 1, 0]
        ]
      |> Array2D.map intToBool
    I ->
    case rot of
        Deg0 ->
          Array2D.fromList [
            [0, 0, 0, 0],
            [1, 1, 1, 1],
            [0, 0, 0, 0],
            [0, 0, 0, 0]
            ]
          |> Array2D.map intToBool
        Deg90 ->
          Array2D.fromList [
            [0, 1, 0, 0],
            [0, 1, 0, 0],
            [0, 1, 0, 0],
            [0, 1, 0, 0]
            ]
          |> Array2D.map intToBool
        Deg180 ->
          Array2D.fromList [
            [0, 0, 0, 0],
            [0, 0, 0, 0],
            [1, 1, 1, 1],
            [0, 0, 0, 0]
            ]
          |> Array2D.map intToBool
        Deg270 ->
          Array2D.fromList [
            [0, 0, 1, 0],
            [0, 0, 1, 0],
            [0, 0, 1, 0],
            [0, 0, 1, 0]
            ]
          |> Array2D.map intToBool
    Z ->
      case rot of
        Deg0 ->
          Array2D.fromList [
            [1, 1, 0],
            [0, 1, 1],
            [0, 0, 0]
            ]
          |> Array2D.map intToBool
        Deg90 ->
          Array2D.fromList [
            [0, 1, 0],
            [1, 1, 0],
            [1, 0, 0]
            ]
          |> Array2D.map intToBool
        Deg180 ->
          Array2D.fromList [
            [0, 0, 0],
            [1, 1, 0],
            [0, 1, 1]
            ]
          |> Array2D.map intToBool
        Deg270 ->
          Array2D.fromList [
            [0, 0, 1],
            [0, 1, 1],
            [0, 1, 0]
            ]
          |> Array2D.map intToBool
    S ->
      case rot of
        Deg0 ->
          Array2D.fromList [
            [0, 1, 1],
            [1, 1, 0],
            [0, 0, 0]
            ]
          |> Array2D.map intToBool
        Deg90 ->
          Array2D.fromList [
            [1, 0, 0],
            [1, 1, 0],
            [0, 1, 0]
            ]
          |> Array2D.map intToBool
        Deg180 ->
          Array2D.fromList [
            [0, 0, 0],
            [0, 1, 1],
            [1, 1, 0]
            ]
          |> Array2D.map intToBool
        Deg270 ->
          Array2D.fromList [
            [0, 1, 0],
            [0, 1, 1],
            [0, 0, 1]
            ]
          |> Array2D.map intToBool
    J ->
      case rot of
        Deg0 ->
          Array2D.fromList [
            [1, 0, 0],
            [1, 1, 1],
            [0, 0, 0]
            ]
          |> Array2D.map intToBool
        Deg90 ->
          Array2D.fromList [
            [0, 1, 0],
            [0, 1, 0],
            [1, 1, 0]
            ]
          |> Array2D.map intToBool
        Deg180 ->
          Array2D.fromList [
            [0, 0, 0],
            [1, 1, 1],
            [0, 0, 1]
            ]
          |> Array2D.map intToBool
        Deg270 ->
          Array2D.fromList [
            [0, 1, 1],
            [0, 1, 0],
            [0, 1, 0]
            ]
          |> Array2D.map intToBool
    L ->
      case rot of
        Deg0 ->
          Array2D.fromList [
            [0, 0, 1],
            [1, 1, 1],
            [0, 0, 0]
            ]
          |> Array2D.map intToBool
        Deg90 ->
          Array2D.fromList [
            [1, 1, 0],
            [0, 1, 0],
            [0, 1, 0]
            ]
          |> Array2D.map intToBool
        Deg180 ->
          Array2D.fromList [
            [0, 0, 0],
            [1, 1, 1],
            [1, 0, 0]
            ]
          |> Array2D.map intToBool
        Deg270 ->
          Array2D.fromList [
            [0, 1, 0],
            [0, 1, 0],
            [0, 1, 1]
            ]
          |> Array2D.map intToBool
    T ->
      case rot of
        Deg0 ->
          Array2D.fromList [
            [0, 1, 0],
            [1, 1, 1],
            [0, 0, 0]
            ]
          |> Array2D.map intToBool
        Deg90 ->
          Array2D.fromList [
            [0, 1, 0],
            [1, 1, 0],
            [0, 1, 0]
            ]
          |> Array2D.map intToBool
        Deg180 ->
          Array2D.fromList [
            [0, 0, 0],
            [1, 1, 1],
            [0, 1, 0]
            ]
          |> Array2D.map intToBool
        Deg270 ->
          Array2D.fromList [
            [0, 1, 0],
            [0, 1, 1],
            [0, 1, 0]
            ]
          |> Array2D.map intToBool

intToBool: Int -> Bool
intToBool i =
  if i == 0 then False else True


new : Int -> BrickType -> Brick
new boardWidth brickType =
  let
    angle = Deg0
    shape = {bType = brickType, rot = angle, brickPos = Pos 0 0}
  in
    {bType=brickType, rot=angle, brickPos = Pos ((boardWidth - (width shape)) // 2) 0}

height : Brick -> Int
height = shape >> Array2D.rows

width : Brick -> Int
width = shape >> Array2D.columns

wallKicks: Rotation -> RotationDirection -> BrickType -> List Pos
wallKicks rotation direction brickType =
  case brickType of
    J -> wallKicksJLSTZ rotation direction
    L -> wallKicksJLSTZ rotation direction
    S -> wallKicksJLSTZ rotation direction
    T -> wallKicksJLSTZ rotation direction
    Z -> wallKicksJLSTZ rotation direction
    I -> wallKicksI rotation direction
    O -> [Pos 0 0]

wallKicksJLSTZ : Rotation -> RotationDirection -> List Pos
wallKicksJLSTZ rotation direction =
  case rotation of
    Deg0 ->
      case direction of
        Clockwise ->
          [Pos 0 0, Pos -1 0, Pos -1 1, Pos 0 -2, Pos -1 -2]
        CounterClockwise ->
          [Pos 0 0, Pos 1 0, Pos 1 1, Pos 0 -2, Pos 1 -2]
    Deg90 ->
      [Pos 0 0, Pos -1 0, Pos -1 -1, Pos 0 2, Pos -1 2]
    Deg180 ->
      case direction of
        Clockwise ->
          [Pos 0 0, Pos 1 0, Pos 1 1, Pos 0 -2, Pos 1 -2]
        CounterClockwise ->
          [Pos 0 0, Pos -1 0, Pos -1 1, Pos 0 -2, Pos -1 -2]
    Deg270 ->
      [Pos 0 0, Pos 1 0, Pos 1 -1, Pos 0 2, Pos 1 2]

wallKicksI : Rotation -> RotationDirection -> List Pos
wallKicksI rotation direction =
  case rotation of
    Deg0 ->
      case direction of
        Clockwise ->
          [Pos 0 0, Pos -2 0, Pos 1 0, Pos -2 -1, Pos 1 2] 
        CounterClockwise ->
          [Pos 0 0, Pos -1 0, Pos 2 0, Pos -1 2, Pos 2 -1]
    Deg90 ->
      case direction of
          Clockwise ->
            [Pos 0 0, Pos 1 0, Pos -2 0, Pos 1 -2, Pos -2 1]
          CounterClockwise ->
            [Pos 0 0, Pos -2 0, Pos 1 0, Pos -2 -1, Pos 1 2]
    Deg180 ->
      case direction of
          Clockwise ->
            [Pos 0 0, Pos 2 0, Pos -1 0, Pos 2 1, Pos -1 -2]
          CounterClockwise ->
            [Pos 0 0, Pos 1 0, Pos -2 0, Pos 1 -2, Pos -2 1]
    Deg270 ->
      case direction of
          Clockwise ->
            [Pos 0 0, Pos -1 0, Pos 2 0, Pos -1 2, Pos 2 -1]
          CounterClockwise ->
            [Pos 0 0, Pos 2 0, Pos -1 0, Pos 2 1, Pos -1 -2]