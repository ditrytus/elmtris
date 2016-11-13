module Brick exposing (..)

import Array2D

type BrickType
  = O | I | Z | S | J | L | T

intToBrickType : number -> BrickType
intToBrickType i =
  case i of
    1 -> O
    2 -> I
    3 -> Z
    4 -> S
    5 -> J
    6 -> L
    _ -> T

type Rotation
  = Deg0
  | Deg90
  | Deg180
  | Deg270

rotate : Rotation -> Rotation
rotate rot =
  case rot of
    Deg0 -> Deg270
    Deg90 -> Deg0
    Deg180 -> Deg90 
    Deg270 -> Deg180

type alias Pos = {x:Int, y:Int}

type alias Brick =
  { bType: BrickType
  , rot: Rotation
  , brickPos: Pos
  }

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

isAt : Int -> Int -> Brick -> Bool
isAt row col brick =
  brick
  |> shape 
  |> Array2D.get row col
  |> Maybe.withDefault False