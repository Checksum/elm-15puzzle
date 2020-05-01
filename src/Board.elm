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
    { board = Dict.empty
    , empty = ( 0, 0 )
    }


indexToPos : Int -> Pos
indexToPos index =
    let
        row =
            index // size

        col =
            remainderBy size index
    in
    ( row, col )


defaultTiles : List (Maybe Tile)
defaultTiles =
    Nothing :: List.map Just (List.range 1 (size ^ 2 - 1))


generator : Generator Board
generator =
    Random.map fromList (shuffle defaultTiles)


positions : List Pos
positions =
    let
        rowPositions : Row -> List ( Row, Col )
        rowPositions row =
            List.map (pos row) (List.range 0 (size - 1))

        pos : Row -> Col -> ( Row, Col )
        pos row column =
            ( row, column )
    in
    List.concatMap rowPositions (List.range 0 (size - 1))


fromList : List (Maybe Tile) -> Board
fromList tiles =
    let
        emptyIndex =
            Maybe.withDefault 0 (elemIndex Nothing tiles)

        emptyPosition =
            indexToPos emptyIndex

        board =
            List.map2 Tuple.pair positions tiles |> Dict.fromList
    in
    Board board emptyPosition


toList : Board -> List ( Pos, Maybe Tile )
toList { board } =
    Dict.toList board


toTileList : Board -> List (Maybe Tile)
toTileList =
    toList >> List.map Tuple.second



-- Check for solvability
-- https://www.cs.bham.ac.uk/~mdr/teaching/modules04/java2/TilesSolvability.html
-- ( (grid width odd) && (#inversions even) )  ||  ( (grid width even) && ((blank on odd row from bottom) == (#inversions even)) )


isSolvable : Board -> Bool
isSolvable board =
    let
        inversions =
            countInversions (toTileList board)

        emptyRow =
            Tuple.first board.empty

        isEven : Int -> Bool
        isEven num =
            remainderBy 2 num == 0

        isOdd =
            not << isEven
    in
    if isOdd size then
        isEven inversions

    else
        (isOdd emptyRow && isEven inversions) || (isEven emptyRow && isOdd inversions)



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


type Direction
    = Up
    | Down
    | Left
    | Right


move : Pos -> Board -> Board
move pos board =
    if pos == board.empty then
        board

    else
        case moveDirection pos board.empty of
            -- Invalid move, do nothing
            Nothing ->
                board

            -- If valid move, swap empty and tile positions
            _ ->
                Board (swap pos board.empty board.board) pos



-- isAdjacent determines if a tile can be moved
-- To be movable, the tile has to be in the same row or same column
-- as the empty piece and should be previous or next tile in either
-- direction
-- Yea, ran into yet another elm compiler bug which hasn't been fixed in a year and half
-- https://github.com/elm/compiler/issues/1773


moveDirection : Pos -> Pos -> Maybe Direction
moveDirection ( tileX, tileY ) ( emptyX, emptyY ) =
    if tileX == emptyX then
        let
            diff =
                tileY - emptyY
        in
        if diff == 1 then
            Just Left

        else if diff == -1 then
            Just Right

        else
            Nothing

    else if tileY == emptyY then
        let
            diff =
                tileX - emptyX
        in
        if diff == 1 then
            Just Up

        else if diff == -1 then
            Just Down

        else
            Nothing

    else
        Nothing


swap : comparable -> comparable -> Dict comparable v -> Dict comparable v
swap k1 k2 dict =
    case ( Dict.get k1 dict, Dict.get k2 dict ) of
        ( Just val1, Just val2 ) ->
            Dict.insert k1 val2 (Dict.insert k2 val1 dict)

        _ ->
            dict
