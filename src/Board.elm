module Board exposing (..)

import Dict exposing (Dict)
import List
import Random exposing (Generator)
import Random.List exposing (shuffle)

type alias Row = Int

type alias Col = Int

type alias Pos = (Row, Col)

type alias Tile = Int

type alias Board = Dict Pos (Maybe Tile)

-- 4x4
size : Int
size = 4


emptyBoard : Board
emptyBoard =
    Dict.empty

-- 1 - 15 with one empty cell
defaultTiles : List (Maybe Tile)
defaultTiles =
    Nothing :: List.map Just (List.range 1 (size ^ 2 - 1))


generator : Generator Board
generator = 
    Random.map generateBoard (shuffle defaultTiles)


generateBoard : List (Maybe Tile) -> Board
generateBoard tiles =
    let
        rows = List.range 0 (size - 1)
        cols = List.range 0 (size - 1)
        positions = List.map2 Tuple.pair rows cols
    in
    List.map2 Tuple.pair positions tiles |> Dict.fromList

