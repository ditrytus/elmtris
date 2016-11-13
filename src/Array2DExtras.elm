module Array2DExtras exposing (..)

import Array
import Array2D

flattenArray2D: Array2D.Array2D a -> Array.Array a
flattenArray2D array2D =
  array2D.data
  |> Array.map (\row -> Array.toList row)
  |> Array.toList
  |> List.concat
  |> Array.fromList