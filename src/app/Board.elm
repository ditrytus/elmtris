module Board exposing (..)

import Array
import Array2D
import Brick exposing (Brick, BrickType)

columns : Int
columns = 10

rows : Int
rows = 22

obstructedRows : Int
obstructedRows = 2

visibleRows : Int
visibleRows = rows - obstructedRows

type alias Cell = Maybe BrickType

cellToBool : Cell -> Bool
cellToBool cell =
  case cell of
  Just _ -> True
  Nothing -> False

type alias Board = Array2D.Array2D Cell

new : Int -> Int -> Board
new r c = Array2D.repeat r c Nothing

empty : Board
empty = new rows columns

skipRows : Int -> Board -> Board
skipRows n board =
  case n of
    0 -> board
    i -> skipRows (n-1) (Array2D.deleteRow 0 board)  
   
removeLines: Board -> Board
removeLines board =
  let
    remainingRows = board.data
      |> Array.filter (\row ->
        row
        |> Array.toList
        |> List.map cellToBool
        |> List.all identity
        |> not)
      |> Array.toList 
  in
    remainingRows
    |> List.append (List.repeat (rows - (List.length remainingRows)) (Array.repeat columns Nothing))
    |> Array.fromList
    |> Array2D.fromArray

countLines: Board -> Int
countLines board = 
  board.data
    |> Array.filter (\row ->
      row
      |> Array.toList
      |> List.map cellToBool
      |> List.all identity)
    |> Array.length