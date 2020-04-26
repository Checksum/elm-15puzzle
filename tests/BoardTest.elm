module BoardTest exposing (all)

import Board
import Expect
import Test exposing (..)


stringToList : String -> List (Maybe Board.Tile)
stringToList str =
    String.split " " str
        |> List.map String.toInt


all : Test
all =
    describe "Board"
        [ describe "indexToPos"
            [ test "indexToPos == (0, 1)" <|
                \_ -> Expect.equal ( 0, 1 ) (Board.indexToPos 1)
            , test "indexToPos == (1, 0)" <|
                \_ -> Expect.equal ( 1, 0 ) (Board.indexToPos 4)
            , test "indexToPos == (2, 3)" <|
                \_ -> Expect.equal ( 2, 3 ) (Board.indexToPos 11)
            , test "indexToPos == (3, 3)" <|
                \_ -> Expect.equal ( 3, 3 ) (Board.indexToPos 15)
            ]
        , describe "countInversions"
            [ test "inversion == 49" <|
                \_ ->
                    Expect.equal 49 (Board.countInversions (stringToList "12 1 10 2 7 11 4 14 5 - 9 15 8 13 6 3"))
            , test "inversion == 48" <|
                \_ ->
                    Expect.equal 48 (Board.countInversions (stringToList "12 1 10 2 7 - 4 14 5 11 9 15 8 13 6 3"))
            ]
        , describe "isSolvable"
            [ test "grid is solvable" <|
                \_ -> Expect.equal True (Board.isSolvable <| Board.fromList <| stringToList "6 1 10 2 7 11 4 14 5 - 9 15 8 12 13 3")
            , test "grid is unsolvable" <|
                \_ -> Expect.equal False (Board.isSolvable <| Board.fromList <| stringToList "1 2 3 4 5 6 7 8 9 10 11 12 13 15 14 -")
            ]
        ]
