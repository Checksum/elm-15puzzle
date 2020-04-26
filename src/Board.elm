module Board exposing (..)

import Dict exposing (Dict)
import List
import List.Extra exposing (elemIndex)
import Random exposing (Generator)
import Random.List exposing (shuffle)


type alias Row =
    Int


type alias Col =
    Int


type alias Pos =
    ( Row, Col )


type alias Tile =
    Int


type alias Board =
    { board : Dict Pos (Maybe Tile)
    , empty : Pos
    }



-- 4x4


size : Int
size =
    4


emptyBoard : Board
emptyBoard =
    { empty = ( 0, 0 )
    , board = Dict.empty
    }


indexToPos : Int -> Pos
indexToPos index =
    let
        row =
            (index // size) - 1

        col =
            remainderBy index size - 1
    in
    ( row, col )


defaultTiles : List (Maybe Tile)
defaultTiles =
    Nothing :: List.map Just (List.range 1 (size ^ 2 - 1))


generator : Generator Board
generator =
    Random.map generateBoard (shuffle defaultTiles)


generateBoard : List (Maybe Tile) -> Board
generateBoard tiles =
    let
        rows =
            List.range 0 (size - 1)

        cols =
            List.range 0 (size - 1)

        positions =
            List.map2 Tuple.pair rows cols

        emptyIndex =
            Maybe.withDefault 0 (elemIndex Nothing tiles)

        emptyPosition =
            indexToPos emptyIndex

        board =
            List.map2 Tuple.pair positions tiles |> Dict.fromList
    in
    Board board emptyPosition


toList : Board -> List (Maybe Tile)
toList { board } =
    Dict.toList board
        |> List.map Tuple.second



-- Check for solvability
-- https://www.cs.bham.ac.uk/~mdr/teaching/modules04/java2/TilesSolvability.html
-- ( (grid width odd) && (#inversions even) )  ||  ( (grid width even) && ((blank on odd row from bottom) == (#inversions even)) )


isSolvable : Board -> Bool
isSolvable board =
    let
        inversions =
            countInversions (toList board)

        emptyRow =
            Tuple.first board.empty

        isEven : Int -> Bool
        isEven num =
            remainderBy num 2 == 0

        isOdd =
            not << isEven
    in
    if isOdd size then
        isEven inversions

    else
        (isOdd emptyRow && isOdd inversions) || (isEven emptyRow && isEven inversions)



-- An inversion is when a tile precedes another tile with a lower number on it. The solution state has zero inversions.


countInversions : List (Maybe Tile) -> Int
countInversions tiles =
    let
        list =
            List.filterMap identity tiles
    in
    list
        |> List.indexedMap (\index tile -> inversion index tile list)
        |> List.foldl (+) 0


inversion : Int -> Tile -> List Tile -> Int
inversion index tile list =
    List.drop (index + 1) list
        |> List.filter (\i -> i < tile)
        |> List.length