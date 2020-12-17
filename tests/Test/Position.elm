module Test.Position exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Move
import Piece
import Player
import Position
import Square
import Test exposing (..)


suite : Test
suite =
    describe "Position" <|
        [ describe "legalMoves"
            [ test "There should be 20 legal moves from the initial position" <|
                \_ ->
                    let
                        expected =
                            20

                        result =
                            let
                                setup =
                                    Position.init Position.defaultSetup
                            in
                            setup
                                |> Position.legalMoves
                                |> List.length
                    in
                    Expect.equal expected result
            ]
        ]
