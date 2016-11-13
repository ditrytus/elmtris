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
  = Vertical VerticalRotation
  | Horizontal HorizontalRotation

type VerticalRotation
  = Deg90
  | Deg270

type HorizontalRotation
  = Deg0
  | Deg180

rotate : Rotation -> Rotation
rotate rot =
  case rot of
    Horizontal Deg0 -> Vertical Deg270
    Vertical Deg90 -> Horizontal Deg0
    Horizontal Deg180 -> Vertical Deg90 
    Vertical Deg270 -> Horizontal Deg180

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
      Array2D.fromList [[True, True], [True, True]]
    I ->
      case rot of
        Horizontal _ ->
          Array2D.fromList [[True, True, True, True]]
        Vertical _ ->
          Array2D.fromList [[True],[True],[True],[True]]
    Z ->
      case rot of
        Vertical _ ->
          Array2D.fromList [[False, True], [True, True], [True, False]]
        Horizontal _ ->
          Array2D.fromList [[True, True, False], [False, True, True]]
    S ->
      case rot of
        Vertical _ ->
          Array2D.fromList [[True, False], [True, True], [False, True]]
        Horizontal _ ->
          Array2D.fromList [[False, True, True], [True, True, False]]    
    J ->
      case rot of
        Horizontal Deg0 ->
          Array2D.fromList [[True, True, True], [False, False, True]]
        Horizontal Deg180 ->
          Array2D.fromList [[True, False, False], [True, True, True]]
        Vertical Deg90 ->
          Array2D.fromList [[True, True], [True, False], [True, False]]
        Vertical Deg270 ->
          Array2D.fromList [[False, True], [False, True], [True, True]]
    L ->
      case rot of
        Horizontal Deg0 ->
          Array2D.fromList [[True, True, True], [True, False, False]]
        Horizontal Deg180 ->
          Array2D.fromList [[False, False, True], [True, True, True]]
        Vertical Deg90 ->
          Array2D.fromList [[True, False], [True, False], [True, True]]
        Vertical Deg270 ->
          Array2D.fromList [[True, True], [False, True], [False, True]]
    T ->
      case rot of
        Horizontal Deg0 ->
          Array2D.fromList [[False, True, False], [True, True, True]]
        Horizontal Deg180 ->
          Array2D.fromList [[True, True, True], [False, True, False]]
        Vertical Deg90 ->
          Array2D.fromList [[False, True], [True, True], [False, True]]
        Vertical Deg270 ->
          Array2D.fromList [[True, False], [True, True], [True, False]]

new : Int -> BrickType -> Brick
new boardWidth brickType =
  let
    angle = Horizontal Deg0
    shape = {bType = brickType, rot = angle, brickPos = Pos 0 0}
  in
    {bType=brickType, rot=angle, brickPos = Pos ((boardWidth - (width shape)) // 2) (height shape)}

height : Brick -> Int
height = shape >> Array2D.rows

width : Brick -> Int
width = shape >> Array2D.columns

originX : Brick -> Int
originX brick = brick.brickPos.x

originY : Brick -> Int
originY brick = brick.brickPos.y - height brick