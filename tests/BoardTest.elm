module BoardTest exposing (all)

import Board
import Expect
import Test exposing (..)


toList : String -> List (Maybe Board.Tile)
toList str =
    String.split " " str
        |> List.filterMap String.toInt
        |> List.map Just


all : Test
all =
    describe "Board"
        [ describe "indexToPos"
            [ test "indexToPos == (0, 3)" <|
                \_ -> Expect.equal ( 0, 3 ) (Board.indexToPos 4)
            , test "indexToPos == (2, 2)" <|
                \_ -> Expect.equal ( 2, 2 ) (Board.indexToPos 11)
            , test "indexToPos == (3, 3)" <|
                \_ -> Expect.equal ( 3, 3 ) (Board.indexToPos 15)
            ]
        , describe "countInversions"
            [ test "inversion == 49" <|
                \_ ->
                    Expect.equal 49 (Board.countInversions (toList "12 1 10 2 7 11 4 14 5 9 15 8 13 6 3"))
            , test "inversion == 48" <|
                \_ ->
                    Expect.equal 48 (Board.countInversions (toList "12 1 10 2 7 4 14 5 11 9 15 8 13 6 3"))
            ]
        ]
