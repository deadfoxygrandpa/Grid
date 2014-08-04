module Grid where

import Array

type Grid a = { grid : Array.Array (Array.Array a), size : Size }
type Size = { width : Int, height : Int }
type Coordinate = { x : Int, y : Int }

initialize : Size -> a -> Grid a
initialize ({width, height} as size) a = Grid (Array.repeat height . Array.repeat width <| a) size

set : Coordinate -> a -> Grid a -> Grid a
set {x, y} a grid =
    if | x < 0 -> grid
       | y < 0 -> grid
       | x >= grid.size.width  -> grid
       | y >= grid.size.height -> grid
       | otherwise -> let row = Array.getOrFail x grid.grid
                          row' = Array.set y a row
                      in  {grid | grid <- Array.set x row' grid.grid}

get : Coordinate -> Grid a -> Maybe a
get {x, y} grid =
    case Array.get y grid.grid of
        Nothing  -> Nothing
        Just row -> Array.get x row

getOrFail : Coordinate -> Grid a -> a
getOrFail {x, y} grid =
    let row = Array.getOrFail y grid.grid
    in  Array.getOrFail x row

getOrElse : a -> Coordinate -> Grid a -> a
getOrElse default coordinate grid =
    case get coordinate grid of
        Nothing -> default
        Just a  -> a
