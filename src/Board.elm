module Board exposing (..)

import Array
import Array2D

width : Int
width = 10

height : Int
height = 20

type alias Board = Array2D.Array2D Bool

empty : Board
empty = Array2D.repeat width height False

removeLines: Board -> Board
removeLines board =
  let
    remainingRows = board.data
      |> Array.filter (\row ->
        row
        |> Array.toList
        |> List.all identity
        |> not)
      |> Array.toList 
  in
    remainingRows
    |> List.append (List.repeat (height - (List.length remainingRows)) (Array.repeat width False))
    |> Array.fromList
    |> Array2D.fromArray